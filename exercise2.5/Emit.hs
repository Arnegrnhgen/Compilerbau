module Emit where

import Codegen
import qualified AbsCPP as S
import ErrM

import Control.Monad (forM, foldM, foldM_)

import qualified LLVM.AST as LAST


initModule :: LAST.Module
initModule = emptyModule "my cool jit"


codegen :: S.Program -> Err ()
codegen (S.PDefs defs) = mapM codegenTop defs


{-codegen :: AST.Module -> Program -> IO AST.Module
codegen modo (PDefs defs) = do
  let modn = mapM codegenTop fns
      ast = runLLVM modo modn
  return ast-}


convertType :: S.Type -> LAST.Type
convertType S.Type_void = void
convertType S.Type_bool = bool
convertType S.Type_int = int
convertType S.Type_double = double
convertType (S.TypeId ident) = undefined --TODO structs


codegenTop :: S.Def -> LLVM ()
--codegenTop (S.Function name args body) = do
codegenTop (S.DFun rettyp (S.Id ident) args stmts) = do
  define (convertType rettyp) ident largs bls
  where
    largs = map (\(S.ADecl typ' (S.Id ident')) -> (convertType typ', LAST.Name ident')) args
    bls = createBlocks $ execCodegen [] $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \(S.ADecl typ' (S.Id ident')) -> do
        var <- alloca (convertType typ')
        store var (local (LAST.Name ident'))
        assign ident' var
      cgen_stmts stmts

codegenTop (S.DStruct _ _) = undefined


cgen_stmts :: [S.Stm] -> Codegen ()
cgen_stmts = mapM_ cgen_stmt

cgen_stmt :: S.Stm -> Codegen ()
cgen_stmt (S.SReturn e) = ret (cgen_exp e) >> return ()
cgen_stmt _ = undefined


cgen_exp :: S.Exp -> LAST.Operand
cgen_exp _ = undefined
