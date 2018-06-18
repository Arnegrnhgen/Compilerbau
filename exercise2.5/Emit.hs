module Emit where

import Codegen
import qualified AbsCPP as S
--import ErrM
import PrintCPP

--import Control.Monad (forM)
import Control.Monad.Except

import qualified LLVM.AST as LAST
import qualified LLVM.AST.Constant as LASTC
--import qualified LLVM.Prelude as LPRE
import qualified LLVM.Module as LMOD
import qualified LLVM.AST.Float as LASTF
import qualified LLVM.Internal.Context as LINTC
import qualified LLVM.AST.IntegerPredicate as LASTIP
--import qualified LLVM.AST.FloatingPointPredicate as LASTFP



liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: LAST.Module -> S.Program -> IO String
codegen modo (S.PDefs fns) = LINTC.withContext $ \context ->
  liftError $ LMOD.withModuleFromAST context newast $ \m -> LMOD.moduleLLVMAssembly m
  where
    modn    = mapM codegenTop fns
    newast = runLLVM modo modn


convertType :: S.Type -> LAST.Type
convertType S.Type_void = Codegen.void
convertType S.Type_bool = Codegen.bool
convertType S.Type_int = Codegen.int
convertType S.Type_double = Codegen.double
convertType (S.TypeId (S.Id ident)) = LAST.NamedTypeReference (LAST.Name ident)


codegenTop :: S.Def -> LLVM ()
codegenTop (S.DFun rettyp (S.Id ident) args stmts) = define (convertType rettyp) ident largs bls
  where
    largs = map (\(S.ADecl typ' (S.Id ident')) -> (convertType typ', LAST.Name ident')) args
    bls = createBlocks $ execCodegen [] $ do
      entryName <- addBlock entryBlockName
      _ <- setBlock entryName
      forM_ args $ \(S.ADecl typ' (S.Id ident')) -> do
        var <- alloca (convertType typ')
        _ <- store var (local (LAST.Name ident'))
        assign ident' var
      cgenStmts stmts
      --ret $ one
codegenTop (S.DStruct (S.Id ident) fields) = structDefine ident types
  where
    types = map (\(S.FDecl typ _) -> convertType typ) fields


cgenStmts :: [S.Stm] -> Codegen ()
cgenStmts = mapM_ cgenStmt

cgenStmt :: S.Stm -> Codegen ()
cgenStmt (S.SReturn e) = do
                            ecode <- cgenExp e
                            _ <- ret ecode
                            return ()
cgenStmt (S.SDecls typ ids) = forM_ ids $ \(S.Id ident) -> do
                                    var <- alloca (convertType typ)
                                    assign ident var
cgenStmt (S.SInit typ (S.Id ident) expr) = do
                                      var <- alloca (convertType typ)
                                      assign ident var
                                      val <- cgenExp expr
                                      _ <- store var val
                                      return ()
cgenStmt (S.SExp expr) = do
                           _ <- cgenExp expr
                           return ()
cgenStmt (S.SBlock stmts) = do
                              st <- getsymtab
                              mapM_ cgenStmt stmts
                              restoresymtab st
cgenStmt (S.SIfElse conexpr stm1 stm2) = do
                                        ifthen <- addBlock "if.then"
                                        ifelse <- addBlock "if.else"
                                        ifexit <- addBlock "if.exit"

                                        -- %entry
                                        cond <- cgenExp conexpr
                                        --unless (typ == S.Type_bool) $ error $ "invalid cp type: " ++ printTree typ
                                        --test <- icmp LASTIP.NE false cond
                                        _ <- cbr cond ifthen ifelse -- Branch based on the condition

                                        -- if.then
                                        _ <- setBlock ifthen
                                        cgenStmt stm1       -- Generate code for the true branch
                                        _ <- br ifexit                    -- Branch to the merge block

                                        -- if.else
                                        _ <- setBlock ifelse
                                        cgenStmt stm2      -- Generate code for the false branch
                                        _ <- br ifexit                   -- Branch to the merge block

                                        -- if.exit
                                        _ <- setBlock ifexit
                                        --phi double [(trval, ifthen), (flval, ifelse)]

                                        return ()
cgenStmt (S.SWhile conexpr stm) = do
                                    forentry <- addBlock "for.entry"
                                    forloop <- addBlock "for.loop"
                                    forexit <- addBlock "for.exit"

                                    _ <- br forentry

                                    _ <- setBlock forentry
                                    cond <- cgenExp conexpr
                                    _ <- cbr cond forloop forexit

                                    _ <- setBlock forloop
                                    cgenStmt stm
                                    _ <- br forentry

                                    _ <- setBlock forexit
                                    return ()

cgenExp :: S.Exp -> Codegen LAST.Operand
cgenExp (S.ETyped (S.EDouble d) _) = return $ cons $ LASTC.Float (LASTF.Double d)
cgenExp (S.ETyped (S.EInt i) _) = return $ cons $ LASTC.Int 32 i
cgenExp (S.ETyped (S.EPlus e1 e2) typ) = do
                                           c1 <- cgenExp e1
                                           c2 <- cgenExp e2
                                           case typ of
                                             S.Type_int -> iadd c1 c2
                                             S.Type_double -> fadd c1 c2
                                             _ -> error $ "CODEGEN ERROR: invalid add typ: " ++ printTree typ
cgenExp (S.ETyped (S.EMinus e1 e2) typ) = do
                                          c1 <- cgenExp e1
                                          c2 <- cgenExp e2
                                          case typ of
                                            S.Type_int -> isub c1 c2
                                            S.Type_double -> fsub c1 c2
                                            _ -> error $ "CODEGEN ERROR: invalid sub typ: " ++ printTree typ
cgenExp (S.ETyped (S.ETimes e1 e2) typ) = do
                                          c1 <- cgenExp e1
                                          c2 <- cgenExp e2
                                          case typ of
                                            S.Type_int -> imul c1 c2
                                            S.Type_double -> fmul c1 c2
                                            _ -> error $ "CODEGEN ERROR: invalid mmul typ: " ++ printTree typ
cgenExp (S.ETyped (S.EDiv e1 e2) typ) = do
                                           c1 <- cgenExp e1
                                           c2 <- cgenExp e2
                                           case typ of
                                             S.Type_int -> idiv c1 c2
                                             S.Type_double -> fdiv c1 c2
                                             _ -> error $ "CODEGEN ERROR: invalid div typ: " ++ printTree typ
cgenExp (S.ETyped (S.EId (S.Id ident)) _) = do
                                                var <- getvar ident
                                                load var
cgenExp (S.ETyped (S.EAss (S.ETyped (S.EId (S.Id ident)) _) rhs) _) = do
                                                                             rhs_code <- cgenExp rhs
                                                                             var <- getvar ident
                                                                             _ <- store var rhs_code
                                                                             return rhs_code
cgenExp (S.ETyped (S.EEq lhs rhs) typ) = do
                                           lcode <- cgenExp lhs
                                           rcode <- cgenExp rhs
                                           case typ of
                                             S.Type_int -> icmp LASTIP.EQ lcode rcode
                                             S.Type_bool -> icmp LASTIP.EQ lcode rcode
                                             _ -> error $ "TODO ERROR: invalid eq cmp type: " ++ printTree typ
cgenExp (S.ETyped (S.ENEq lhs rhs) typ) = do
                                            lcode <- cgenExp lhs
                                            rcode <- cgenExp rhs
                                            case typ of
                                              S.Type_int -> icmp LASTIP.NE lcode rcode
                                              S.Type_bool -> icmp LASTIP.NE lcode rcode
                                              _ -> error $ "TODO ERROR: invalid ne cmp type: " ++ printTree typ
cgenExp (S.ETyped (S.EApp (S.Id ident) argexprs) _) = do
                                                          argcodes <- mapM cgenExp argexprs
                                                          call (externf (LAST.Name ident)) argcodes
cgenExp (S.ETyped S.EFalse _) = return false
cgenExp (S.ETyped S.ETrue _) = return true



cgenExp e@(S.ETrue) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EFalse) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EInt _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EDouble _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EId _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EApp _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EProj _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EPIncr _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EPDecr _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EIncr _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EDecr _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.ETimes _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EDiv _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EPlus _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EMinus _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.ELt _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EGt _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.ELtEq _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EGtWq _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EEq _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.ENEq _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EAnd _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EOr _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EPrAss _ _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
cgenExp e@(S.EAss _ _) = error $ "CODEGEN ERROR: untyped expression not supported: " ++ show e ++ " ::: " ++ printTree e
--cgenExp e = error $ "TODO ERROR: not implemented: " ++ show e ++ " ::: " ++ printTree e
