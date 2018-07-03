module Emit where

import Codegen
import qualified AbsCPP as S
--import ErrM
import PrintCPP
import TypeChecker

import qualified Data.Map as Map

--import Control.Monad (forM)
import Control.Monad.Except

import qualified LLVM.AST as LAST
import qualified LLVM.AST.Constant as LASTC
--import qualified LLVM.Prelude as LPRE
import qualified LLVM.Module as LMOD
import qualified LLVM.AST.Float as LASTF
import qualified LLVM.Internal.Context as LINTC
import qualified LLVM.AST.IntegerPredicate as LASTIP
import qualified LLVM.AST.FloatingPointPredicate as LASTFP


liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: LAST.Module -> S.Program -> Structs -> IO String
codegen modo (S.PDefs fns) structs = LINTC.withContext $ \context ->
  liftError $ LMOD.withModuleFromAST context newast $ \m -> LMOD.moduleLLVMAssembly m
  where
    modn    = mapM (codegenTop structs) fns
    newast = runLLVM modo modn


convertType :: S.Type -> LAST.Type
convertType S.Type_void = Codegen.void
convertType S.Type_bool = Codegen.bool
convertType S.Type_int = Codegen.int
convertType S.Type_double = Codegen.double
convertType (S.TypeId (S.Id ident)) = LAST.NamedTypeReference (LAST.Name ident)


codegenTop :: Structs -> S.Def -> LLVM ()
codegenTop structs (S.DFun rettyp (S.Id ident) args stmts) = define (convertType rettyp) ident largs bls
  where
    largs = map (\(S.ADecl typ' (S.Id ident')) -> (convertType typ', LAST.Name ident')) args
    bls = createBlocks $ execCodegen [] structs $ do
      entryName <- addBlock entryBlockName
      _ <- setBlock entryName
      forM_ args $ \(S.ADecl typ' (S.Id ident')) -> do
        var <- alloca (convertType typ')
        _ <- store var (local (LAST.Name ident') (convertType typ'))
        assign ident' var
      cgenStmts stmts
      ret $ defaultValue rettyp
codegenTop _ (S.DStruct (S.Id ident) fields) = structDefine ident types
  where
    types = map (\(S.FDecl typ _) -> convertType typ) fields


defaultValue :: S.Type -> Maybe LAST.Operand
defaultValue S.Type_bool = Just false
defaultValue S.Type_int = Just intZero
defaultValue S.Type_double = Just doubleZero
defaultValue S.Type_void = Nothing
defaultValue (S.TypeId _) = error $ "CODEGEN ERROR: missing return in function returning a struct"

cgenStmts :: [S.Stm] -> Codegen ()
cgenStmts list = mapM_ cgenStmt list

cgenStmt :: S.Stm -> Codegen ()
cgenStmt (S.SReturn e) = do
                            ecode <- cgenExp e
                            _ <- ret $ Just ecode
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
cgenExp e@(S.ETyped (S.EAss _ _) _) = error $ "CODEGEN ERROR: unsupported assigment lhs: " ++ show e ++ " ::: " ++ printTree e
cgenExp (S.ETyped (S.EEq lhs rhs) typ) = do
                                           lcode <- cgenExp lhs
                                           rcode <- cgenExp rhs
                                           case typ of
                                             S.Type_int -> icmp LASTIP.EQ lcode rcode
                                             S.Type_bool -> icmp LASTIP.EQ lcode rcode
                                             S.Type_double -> do
                                                                lcode' <- convertDouble lcode
                                                                rcode' <- convertDouble rcode
                                                                fcmp LASTFP.OEQ lcode' rcode'
                                             _ -> error $ "CODEGEN ERROR: invalid eq cmp type: " ++ printTree typ
cgenExp (S.ETyped (S.ENEq lhs rhs) typ) = do
                                            lcode <- cgenExp lhs
                                            rcode <- cgenExp rhs
                                            case typ of
                                              S.Type_int -> icmp LASTIP.NE lcode rcode
                                              S.Type_bool -> icmp LASTIP.NE lcode rcode
                                              S.Type_double -> do
                                                                 lcode' <- convertDouble lcode
                                                                 rcode' <- convertDouble rcode
                                                                 fcmp LASTFP.ONE lcode' rcode'
                                              _ -> error $ "CODEGEN ERROR: invalid ne cmp type: " ++ printTree typ
cgenExp (S.ETyped (S.EApp (S.Id ident) argexprs) typ) = do
                                                          argcodes <- mapM cgenExp argexprs
                                                          call (externf (LAST.Name ident) (convertType typ)) argcodes (convertType typ)
cgenExp (S.ETyped S.EFalse _) = return false
cgenExp (S.ETyped S.ETrue _) = return true
cgenExp (S.ETyped (S.EIncr (S.ETyped (S.EId (S.Id ident)) _)) typ) = do
                                        var <- getvar ident
                                        code <- load var
                                        case typ of
                                            S.Type_int -> do
                                                            add <- iadd code intOne
                                                            _ <- store var add
                                                            return add
                                            S.Type_double -> do
                                                               add <- fadd code doubleOne
                                                               _ <- store var add
                                                               return add
                                            _ -> error $ "CODEGEN ERROR: invalid incr typ: " ++ printTree typ
cgenExp (S.ETyped (S.EIncr _) _) = error $ "CODEGEN ERROR: incr only for identifiers"
cgenExp (S.ETyped (S.EDecr (S.ETyped (S.EId (S.Id ident)) _)) typ) = do
                                          var <- getvar ident
                                          code <- load var
                                          case typ of
                                            S.Type_int -> do
                                                            sub <- isub code intOne
                                                            _ <- store var sub
                                                            return sub
                                            S.Type_double -> do
                                                               sub <- fsub code doubleOne
                                                               _ <- store var sub
                                                               return sub
                                            _ -> error $ "CODEGEN ERROR: invalid decr typ: " ++ printTree typ
cgenExp (S.ETyped (S.EDecr _) _) = error $ "CODEGEN ERROR: decr only for identifiers"
cgenExp (S.ETyped (S.EPIncr (S.ETyped (S.EId (S.Id ident)) _)) typ) = do
                                           var <- getvar ident
                                           code <- load var
                                           case typ of
                                             S.Type_int -> iadd code intOne >>= store var >> return code
                                             S.Type_double -> fadd var doubleOne >>= store var >> return code
                                             _ -> error $ "CODEGEN ERROR: invalid pincr typ: " ++ printTree typ
cgenExp (S.ETyped (S.EPIncr _) _) = error $ "CODEGEN ERROR: post incr only for identifiers"
cgenExp (S.ETyped (S.EPDecr (S.ETyped (S.EId (S.Id ident)) _)) typ) = do
                                            var <- getvar ident
                                            code <- load var
                                            case typ of
                                              S.Type_int -> isub code intOne >>= store var >> return code
                                              S.Type_double -> fsub code doubleOne >>= store var >> return code
                                              _ -> error $ "CODEGEN ERROR: invalid pdecr typ: " ++ printTree typ
cgenExp (S.ETyped (S.EPDecr _) _) = error $ "CODEGEN ERROR: post decr only for identifiers"
cgenExp (S.ETyped (S.ELt lhs rhs) typ) = do
                                           lcode <- cgenExp lhs
                                           rcode <- cgenExp rhs
                                           case typ of
                                             S.Type_int -> icmp LASTIP.SLT lcode rcode
                                             S.Type_double -> do
                                                                lcode' <- convertDouble lcode
                                                                rcode' <- convertDouble rcode
                                                                fcmp LASTFP.OLT lcode' rcode'
                                             _ -> error $ "CODEGEN ERROR: invalid lt typ: " ++ printTree typ
cgenExp (S.ETyped (S.EGt lhs rhs) typ) = do
                                           lcode <- cgenExp lhs
                                           rcode <- cgenExp rhs
                                           case typ of
                                             S.Type_int -> icmp LASTIP.SGT lcode rcode
                                             S.Type_double -> do
                                                                lcode' <- convertDouble lcode
                                                                rcode' <- convertDouble rcode
                                                                fcmp LASTFP.OGT lcode' rcode'
                                             _ -> error $ "CODEGEN ERROR: invalid gt typ: " ++ printTree typ
cgenExp (S.ETyped (S.ELtEq lhs rhs) typ) = do
                                             lcode <- cgenExp lhs
                                             rcode <- cgenExp rhs
                                             case typ of
                                               S.Type_int -> icmp LASTIP.SLE lcode rcode
                                               S.Type_double -> do
                                                                  lcode' <- convertDouble lcode
                                                                  rcode' <- convertDouble rcode
                                                                  fcmp LASTFP.OLE lcode' rcode'
                                               _ -> error $ "CODEGEN ERROR: invalid lteq typ: " ++ printTree typ
cgenExp (S.ETyped (S.EGtWq lhs rhs) typ) = do
                                             lcode <- cgenExp lhs
                                             rcode <- cgenExp rhs
                                             case typ of
                                               S.Type_int -> icmp LASTIP.SGE lcode rcode
                                               S.Type_double -> do
                                                                  lcode' <- convertDouble lcode
                                                                  rcode' <- convertDouble rcode
                                                                  fcmp LASTFP.OGE lcode' rcode'
                                               _ -> error $ "CODEGEN ERROR: invalid gteq typ: " ++ printTree typ
cgenExp (S.ETyped (S.EAnd lhs rhs) _) = do
                                          lcode <- cgenExp lhs
                                          cond <- icmp LASTIP.EQ lcode true
                                          startblock <- getBlock


                                          unlazy <- addBlock "and.unlazy"
                                          exit <- addBlock "and.exit"


                                          _ <- cbr cond unlazy exit

                                          _ <- setBlock unlazy
                                          unlazyblock <- getBlock
                                          rcode <- cgenExp rhs
                                          result <- bAnd lcode rcode
                                          _ <- br exit

                                          _ <- setBlock exit
                                          phi bool [(false, startblock), (result, unlazyblock)]
cgenExp (S.ETyped (S.EOr lhs rhs) _) = do
                                         lcode <- cgenExp lhs
                                         cond <- icmp LASTIP.EQ lcode false
                                         startblock <- getBlock


                                         unlazy <- addBlock "or.unlazy"
                                         exit <- addBlock "or.exit"


                                         _ <- cbr cond unlazy exit

                                         _ <- setBlock unlazy
                                         unlazyblock <- getBlock
                                         rcode <- cgenExp rhs
                                         result <- bOr lcode rcode
                                         _ <- br exit

                                         _ <- setBlock exit
                                         phi bool [(true, startblock), (result, unlazyblock)]
cgenExp (S.ETyped (S.EProj lhs (S.EId memberid)) typ) = do
    --structs <- getStructs
    --var <- getvar structvarident
    --pos <- getElementPtr var (computeStructIndicies structs structtype member)
    pos <- computeStructPosition lhs memberid typ
    load pos
cgenExp (S.ETyped e@(S.EProj _ _) _) = error $ "CODEGEN ERROR: unsupported struct projection: " ++ show e ++ " ::: " ++ printTree e
cgenExp (S.ETyped (S.EPrAss lhs (S.EId member) rhs) typ) = do
    rcode <- cgenExp rhs
    pos <- computeStructPosition lhs member typ
    _ <- store pos rcode
    return rcode
cgenExp (S.ETyped e@(S.EPrAss _ _ _) _) = error $ "CODEGEN ERROR: unsupported struct assignment: " ++ show e ++ " ::: " ++ printTree e

cgenExp e@(S.ETyped (S.ETyped _ _) _) = error $ "CODEGEN ERROR: unsupported double nested typed expression: " ++ show e ++ " ::: " ++ printTree e

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


convertDouble :: LAST.Operand -> Codegen (LAST.Operand)
convertDouble o@(LAST.LocalReference (LAST.IntegerType _) _) = sitofp o
convertDouble o@(LAST.ConstantOperand (LASTC.Int _ _)) = sitofp o
convertDouble o = return o


computeStructIndicies :: Structs -> S.Id -> S.Id -> [LAST.Operand]
computeStructIndicies structs structtype member = [intZero, cons $ LASTC.Int 32 memberidx]
    where
        memberidx = case Map.lookup structtype structs of
                      Nothing -> error $ "CODEGEN ERROR: No such struct found: " ++ printTree structtype
                      Just s -> case Map.lookup member s of
                                  Nothing -> error $ "CODEGEN ERROR: No such member found in struct: " ++ printTree member ++ " in " ++ printTree structtype
                                  Just (_,ident) -> ident


computeStructPosition :: S.Exp -> S.Id -> S.Type -> Codegen LAST.Operand
computeStructPosition (S.ETyped (S.EId (S.Id structvarident)) (S.TypeId structtype)) memberid typ = do
    structs <- getStructs
    var <- getvar structvarident
    getElementPtr var (computeStructIndicies structs structtype memberid) (convertType typ)

computeStructPosition (S.ETyped (S.EProj lhs (S.EId lhsmemberid)) (S.TypeId structtype)) memberid typ = do
    structs <- getStructs
    innerpos <- computeStructPosition lhs lhsmemberid typ
    getElementPtr innerpos (computeStructIndicies structs structtype memberid) (convertType typ)

computeStructPosition e m _ = error $ "CODEGEN ERROR: unsupported struct position: " ++ show e ++ " ::: " ++ printTree e ++ " (at member " ++ printTree m ++ ")"
