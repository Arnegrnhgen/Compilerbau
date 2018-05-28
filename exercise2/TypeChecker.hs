module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import qualified Data.Map as Map
import Control.Monad (foldM, when, foldM_, unless)


type Env = (Sigs, [Context], Structs)
type Sigs = Map.Map Id ([Type],Type)
type Context = Map.Map Id Type
type Structs = Map.Map Id Struct
type Struct = Map.Map Id Type


{-
    TODO:
        check function returntype
-}


lookupVarEnv :: Env -> Id -> Err Type
lookupVarEnv (_, [], _) (Id str) = fail $ "identifier " ++ str ++ " not found"
lookupVarEnv (sigs, ctx:ctxs, structs) ident = case Map.lookup ident ctx of
                                                 Nothing -> lookupVarEnv (sigs, ctxs, structs) ident
                                                 Just x -> return x


lookupFunEnv :: Env -> Id -> Err ([Type], Type)
lookupFunEnv (sigs, _, _) ident = case Map.lookup ident sigs of
                                    Nothing -> fail $ "function identifier " ++ printTree ident ++ " not found"
                                    Just x -> return x


compareArgs :: (Env, Integer) -> (Exp, Type) -> Err (Env, Integer)
compareArgs (env, i) (expr, t) = do
                                 t2 <- inferExpr env expr
                                 when (t /= t2) $ fail $ "function argument number " ++ show i ++ " type mismatch: expected " ++ printTree t2 ++ ", got " ++ printTree t
                                 return (env, i+1)


checkFunArgs :: Env -> [Exp] -> ([Type], Type) -> Err Type
checkFunArgs env exps (args, rettype) = do
                                         when (length exps /= length args) $ fail $ "bad number of function arguments: expected " ++ show (length args) ++ ", got " ++ show (length exps)
                                         foldM_ compareArgs (env, 1) (zip exps args)
                                         return rettype


insertVarEnv :: Type -> Env -> Id -> Err Env
insertVarEnv t (sigs, c:ctxs, structs) i = case Map.insertLookupWithKey (\_ a _ -> a) i t c of
                                             (Nothing, c2) -> return (sigs, c2 : ctxs, structs)
                                             (Just _, _) -> fail $ "variable " ++ printTree i ++ " already defined"
insertVarEnv _ (_, [], _) _ = error "variable insert into empty context, run envNewBlock() before"


insertFunVar :: Env -> Arg -> Err Env
insertFunVar (sigs, c:ctxs, structs) (ADecl t i) = case Map.insertLookupWithKey (\_ a _ -> a) i t c of
                                                     (Nothing, c2) -> return (sigs, c2 : ctxs, structs)
                                                     (Just _, _) -> fail $ "function variable " ++ printTree i ++ " already defined"
insertFunVar (_, [], _) _ = error "function var insert into empty context, run envNewBlock() before"


lookUpStructField :: Env -> Id -> Type -> Err Type
lookUpStructField (_, _, structs) membername (TypeId structname) = case Map.lookup structname structs of
                                                                     Nothing -> fail $ "struct " ++ show structname ++ " not found"
                                                                     Just struct -> case Map.lookup membername struct of
                                                                       Nothing -> fail $ "struct member " ++ show membername ++ " of struct " ++ show structname ++ " not found"
                                                                       Just field -> return field
lookUpStructField (_, _, _) _ structname = fail $ "identifier " ++ printTree structname ++ " is not a struct name"


checkExpType :: Env -> Type -> Exp -> Err ()
checkExpType env typ expr = do
                              typ2 <- inferExpr env expr
                              when (typ2 /= typ) $ fail $ "type of expression " ++ printTree expr ++ " expected to be " ++ printTree typ ++ ", but found " ++ printTree typ2


inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
inferBin types env exp1 exp2 = do
                                 typ1 <- inferExpr env exp1
                                 typ2 <- inferExpr env exp2
                                 unless (typ1 `elem` types) $ fail $ "wrong type of expression " ++ printTree exp1 ++ " for binary operation: " ++ printTree typ1
                                 unless (typ2 `elem` types) $ fail $ "wrong type of expression " ++ printTree exp2 ++ " for binary operation: " ++ printTree typ2
                                 typesCompatible typ1 typ2


inferNumBin :: Env -> Exp -> Exp -> Err Type
inferNumBin env exp1 exp2 = do
                              type1 <- inferExpr env exp1
                              type2 <- inferExpr env exp2
                              _ <- isNumeric type1
                              _ <- isNumeric type2
                              typesCompatible type1 type2


isNumeric :: Type -> Err Type
isNumeric Type_int = return Type_int
isNumeric Type_double = return Type_double
isNumeric t = fail $ "invalid numeric type " ++ printTree t


typesCompatible :: Type -> Type -> Err Type
typesCompatible Type_int Type_int = return Type_int
typesCompatible Type_int Type_double = return Type_double
typesCompatible Type_double Type_int = return Type_double
typesCompatible Type_double Type_double = return Type_double
typesCompatible Type_bool Type_bool = return Type_bool
typesCompatible t1 t2 = fail $ "incompatible types " ++ printTree t1 ++ " and " ++ printTree t2


inferExpr :: Env -> Exp -> Err Type
inferExpr _ ETrue = return Type_bool
inferExpr _ EFalse = return Type_bool
inferExpr _ (EInt _) = return Type_int
inferExpr _ (EDouble _) = return Type_double
inferExpr env (EId ident) = lookupVarEnv env ident
inferExpr env (EApp ident exprs) = lookupFunEnv env ident >>= checkFunArgs env exprs
inferExpr env (EProj lhs (EId rhs)) = inferExpr env lhs >>= lookUpStructField env rhs
inferExpr _ (EProj l r) = fail $ "invalid struct usage: " ++ printTree l ++ "." ++ printTree r
inferExpr env (EPIncr expr) = inferExpr env expr >>= isNumeric
inferExpr env (EPDecr expr) = inferExpr env expr >>= isNumeric
inferExpr env (EIncr expr) = inferExpr env expr >>= isNumeric
inferExpr env (EDecr expr) = inferExpr env expr >>= isNumeric
inferExpr env (ETimes exp1 exp2) = inferNumBin env exp1 exp2
inferExpr env (EDiv exp1 exp2) = inferNumBin env exp1 exp2
inferExpr env (EPlus exp1 exp2) = inferNumBin env exp1 exp2
inferExpr env (EMinus exp1 exp2) = inferNumBin env exp1 exp2
inferExpr env (ELt exp1 exp2) = inferNumBin env exp1 exp2 >> return Type_bool
inferExpr env (EGt exp1 exp2) = inferNumBin env exp1 exp2 >> return Type_bool
inferExpr env (ELtEq exp1 exp2) = inferNumBin env exp1 exp2 >> return Type_bool
inferExpr env (EGtWq exp1 exp2) = inferNumBin env exp1 exp2 >> return Type_bool
inferExpr env (EEq exp1 exp2) = inferBin [Type_int, Type_double, Type_bool] env exp1 exp2 >> return Type_bool
inferExpr env (ENEq exp1 exp2) = inferBin [Type_int, Type_double, Type_bool] env exp1 exp2 >> return Type_bool
inferExpr env (EAnd exp1 exp2) = inferBin [Type_bool] env exp1 exp2 >> return Type_bool
inferExpr env (EOr exp1 exp2) = inferBin [Type_bool] env exp1 exp2 >> return Type_bool
inferExpr env (EPrAss structname membername value) = do
                                                       lhs <- inferExpr env (EProj structname membername)
                                                       rhs <- inferExpr env value
                                                       when (lhs /= rhs) $ fail $ "bad struct assignment from " ++ printTree value ++ " to " ++ printTree structname ++ "." ++ printTree membername ++ " (" ++ printTree rhs ++ " to " ++ printTree lhs ++ ")"
                                                       return lhs
inferExpr env (EAss lhs rhs) = do
                                 typlhs <- inferExpr env lhs
                                 typrhs <- inferExpr env rhs
                                 when (typlhs /= typrhs) $ fail $ "bad assignment from " ++ printTree rhs ++ " to " ++ printTree lhs ++ " (" ++ printTree typrhs ++ " to " ++ printTree typlhs ++ ")"
                                 return typlhs


checkTypeExists :: Env -> Type -> Err ()
checkTypeExists (_, _, structs) (TypeId ident) = case Map.lookup ident structs of
                                                   Nothing -> fail $ "unknown datatype " ++ printTree ident
                                                   _ -> return ()
checkTypeExists _ _ = return ()


checkStm :: Env -> Stm -> Err Env
checkStm env (SReturn e) = do
                             --typ_actual <- inferExpr env e
                             typ_expected <- lookupVarEnv env (Id "__return")
                             checkExpType env typ_expected e

                             --TODO: check with function signature
                             return env
checkStm env (SExp e) = do
                          _ <- inferExpr env e
                          return env
checkStm env (SInit t i e) = do
                               checkTypeExists env t
                               checkExpType env t e
                               insertVarEnv t env i
checkStm env (SDecls t ids) = do
                                checkTypeExists env t
                                foldM (insertVarEnv t) env ids
checkStm env (SWhile e s) = do
                              conditionType <- inferExpr env e
                              when (conditionType /= Type_bool) $ fail $ "bad condition type " ++ printTree e ++ " in while: expected bool, found " ++ printTree conditionType
                              checkStm env s
checkStm env (SBlock ss) = do
                             innerEnv <- envNewBlock env
                             innerEnv2 <- foldM checkStm innerEnv ss
                             envPopBlock innerEnv2
checkStm env (SIfElse e s1 s2) = do
                                   conditionType <- inferExpr env e
                                   when (conditionType /= Type_bool) $ fail $ "bad condition type " ++ printTree e ++ " in if: expected bool, found " ++ printTree conditionType
                                   env1 <- checkStm env s1
                                   checkStm env1 s2


envNewBlock :: Env -> Err Env
envNewBlock (sigs, c, structs) = return (sigs, Map.empty : c, structs)


envPopBlock :: Env -> Err Env
envPopBlock (sigs, c, structs) = return (sigs, tail c, structs)


insertStructField :: Env -> Map.Map Id Type -> Field -> Err (Map.Map Id Type)
insertStructField env m (FDecl typ ident) = do
                                              checkTypeExists env typ
                                              case Map.insertLookupWithKey (\_ a _ -> a) ident typ m of
                                                (Nothing, m2) -> return m2
                                                (Just _, _) -> fail $ "struct member " ++ printTree ident ++ " already defined"


checkDef :: Env -> Def -> Err Env
checkDef env (DFun rettype _ args stmts) = do
                                       env2 <- envNewBlock env
                                       env3 <- insertVarEnv rettype env2 (Id "__return")
                                       env4 <- foldM insertFunVar env3 args
                                       env5 <- foldM checkStm env4 stmts
                                       --mapM_ (checkExpType env5 rettype) [e | SReturn e <- stmts]
                                       --TODO: iterate over stmts if return has correct type
                                       envPopBlock env5
checkDef env (DStruct _ _)  = return env


newEnv :: Env
newEnv = (Map.fromList builtIns, [], Map.empty)
    where builtIns = [(Id "printInt", ([Type_int], Type_void)), (Id "printDouble", ([Type_double], Type_void)), (Id "readInt", ([], Type_int)), (Id "readDouble", ([], Type_double))]


extendSigs :: Env -> Def -> Err Env
extendSigs env@(sigs, var, structs) (DFun returntype i args _) = do
                                                                   checkTypeExists env returntype
                                                                   mapM_ (checkTypeExists env) argtypes
                                                                   case Map.insertLookupWithKey (\_ a _ -> a) i (argtypes, returntype) sigs of
                                                                     (Nothing, sigs2) -> return (sigs2,var,structs)
                                                                     (Just _, _) -> fail $ "function " ++ printTree i ++ " already defined"
                                                                     where argtypes = [t | ADecl t _ <- args]
extendSigs env@(sigs, ctxs, structs) (DStruct ident fields) = case Map.lookup ident structs of
                                                                Nothing -> do
                                                                             f <- foldM (insertStructField env) Map.empty fields
                                                                             return (sigs, ctxs, Map.insert ident f structs)
                                                                Just _ -> fail $ "struct " ++ printTree ident ++ " already defined"


typecheck :: Program -> Err Env
typecheck (PDefs defs) = do
                           sigEnv <- foldM extendSigs newEnv defs
                           foldM checkDef sigEnv defs
