module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import qualified Data.Map as Map
import Control.Monad


type Env = (Sigs, [Context], Structs)
type Sigs = Map.Map Id ([Type],Type)
type Context = Map.Map Id Type
type Structs = Map.Map Id Struct
type Struct = Map.Map Id Type


{-
    TODO:
        check function returntype
-}


lookupCtx :: [Context] -> Id -> Err Type
lookupCtx [] (Id str) = fail $ "identifier " ++ str ++ " not found"
lookupCtx (ctx:ctxs) ident = case (Map.lookup ident ctx) of
                               Nothing -> lookupCtx ctxs ident
                               Just x -> return x


lookupVarEnv :: Env -> Id -> Err Type
lookupVarEnv (_,ctxs,_) ident = lookupCtx ctxs ident


lookupSigs :: Sigs -> Id -> Err ([Type], Type)
lookupSigs sigs (Id str) = case (Map.lookup (Id str) sigs) of
                                  Nothing -> fail $ "function identifier " ++ str ++ " not found"
                                  Just x -> return x


lookupFunEnv :: Env -> Id -> Err ([Type], Type)
lookupFunEnv (sigs,_,_) ident = lookupSigs sigs ident


compareArgs :: (Env,Integer) -> (Exp,Type) -> Err (Env,Integer)
compareArgs (env,i) (expr,t) = do
                                 t2 <- inferExpr env expr
                                 when (t /= t2) $ fail $ "function argument number " ++ show i ++ " type mismatch: " ++ printTree t ++ " vs " ++ printTree t2
                                 return (env,i+1)


checkFunArgs :: Env -> [Exp] -> ([Type],Type) -> Err Type
checkFunArgs env exps (args,rettype) = do
                                         when (length exps /= length args) $ fail $ "bad number of function arguments (" ++ show (length exps) ++ " vs " ++ show (length args) ++ ")"
                                         _ <- foldM compareArgs (env,1) (zip exps args)
                                         return rettype


insertVarEnv :: Env -> Type -> Id -> Env
insertVarEnv (sigs,[],structs) t i       = (sigs,([Map.fromList [(i,t)]]),structs)
insertVarEnv (sigs,(c:ctxs),structs) t i = (sigs,(Map.insert i t c) : ctxs,structs)


insertVarsEnv :: Env -> Type -> [Id] -> Env
insertVarsEnv env _ [] = env
insertVarsEnv env t (i:is) = insertVarsEnv (insertVarEnv env t i) t is


insertFunVar :: Env -> Arg -> Env
insertFunVar (sigs,(c:ctxs),structs) (ADecl t i) = (sigs, (Map.insert i t c) : ctxs,structs)
insertFunVar (_, [], _) _ = error "function insert into empty context"


insertFunVars :: Env -> [Arg] -> Err Env
insertFunVars env [] = return env
insertFunVars env (a:args) = insertFunVars (insertFunVar env a) args


lookUpStructField :: Env -> Id -> Type -> Err Type
lookUpStructField (_,_,structs) membername (TypeId structname) = case Map.lookup structname structs of
                                                                         Nothing -> fail $ "struct " ++ show structname ++ " not found"
                                                                         Just struct -> case Map.lookup membername struct of
                                                                                          Nothing -> fail $ "struct member " ++ show membername ++ " of struct " ++ show structname ++ " not found"
                                                                                          Just field -> return field
lookUpStructField (_,_,_) _ structname = fail $ "identifier " ++ printTree structname ++ " not a struct name"


checkExpType :: Env -> Type -> Exp -> Err ()
checkExpType env typ expr = do
                              typ2 <- inferExpr env expr
                              if typ2 == typ
                                then
                                  return ()
                                else
                                  fail $ "type of " ++ printTree expr ++ " expected " ++ printTree typ ++ " but found " ++ printTree typ2


inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
inferBin types env exp1 exp2 = do
                                 typ <- inferExpr env exp1
                                 if elem typ types
                                   then
                                     do
                                       checkExpType env typ exp2
                                       return typ
                                   else
                                     fail $ "wrong type of expression " ++ printTree exp1


inferNumBin :: Env -> Exp -> Exp -> Err Type
inferNumBin env exp1 exp2 = do
                              type1 <- inferExpr env exp1
                              type2 <- inferExpr env exp2
                              case type1 of
                                Type_int -> case type2 of
                                              Type_int -> return Type_int
                                              Type_double -> return Type_double
                                              _ -> fail $ "wrong type of second numeric expression " ++ printTree exp2
                                Type_double -> case type2 of
                                                 Type_int -> return Type_double
                                                 Type_double -> return Type_double
                                                 _ -> fail $ "wrong type of second numeric expression " ++ printTree exp2
                                _ -> fail $ "wrong type of first numeric expression " ++ printTree exp1


inferExpr :: Env -> Exp -> Err Type
inferExpr _ ETrue = return Type_bool
inferExpr _ EFalse = return Type_bool
inferExpr _ (EInt _) = return Type_int
inferExpr _ (EDouble _) = return Type_double
inferExpr env (EId ident) = lookupVarEnv env ident
inferExpr env (EApp ident exprs) = lookupFunEnv env ident >>= checkFunArgs env exprs
inferExpr env (EProj (EId lhs) (EId rhs)) = lookupVarEnv env lhs >>= lookUpStructField env rhs --TODO allow a.b.c
inferExpr _ (EProj l r) = fail $ "invalid struct usage " ++ printTree l ++ "." ++ printTree r
inferExpr env (EPIncr expr) = inferExpr env expr --TODO: allow only int, double
inferExpr env (EPDecr expr) = inferExpr env expr --TODO: allow only int, double
inferExpr env (EIncr expr) = inferExpr env expr --TODO: allow only int, double
inferExpr env (EDecr expr) = inferExpr env expr --TODO: allow only int, double
inferExpr env (ETimes exp1 exp2) = inferNumBin env exp1 exp2
inferExpr env (EDiv exp1 exp2) = inferNumBin env exp1 exp2
inferExpr env (EPlus exp1 exp2) = inferNumBin env exp1 exp2
inferExpr env (EMinus exp1 exp2) = inferNumBin env exp1 exp2
inferExpr env (ELt exp1 exp2) = inferNumBin env exp1 exp2 >> return Type_bool
inferExpr env (EGt exp1 exp2) = inferNumBin env exp1 exp2 >> return Type_bool
inferExpr env (ELtEq exp1 exp2) = inferNumBin env exp1 exp2 >> return Type_bool
inferExpr env (EGtWq exp1 exp2) = inferNumBin env exp1 exp2 >> return Type_bool
inferExpr env (EEq exp1 exp2) = inferBin [Type_int, Type_double, Type_bool] env exp1 exp2 >> return Type_bool --TODO:allow int double comparison
inferExpr env (ENEq exp1 exp2) = inferBin [Type_int, Type_double, Type_bool] env exp1 exp2 >> return Type_bool --TODO:allow int double comparison
inferExpr env (EAnd exp1 exp2) = inferBin [Type_bool] env exp1 exp2 >> return Type_bool
inferExpr env (EOr exp1 exp2) = inferBin [Type_bool] env exp1 exp2 >> return Type_bool
inferExpr env (EPrAss structname membername value) = do
                                                       lhs <- inferExpr env (EProj structname membername)
                                                       rhs <- inferExpr env value
                                                       when (lhs /= rhs) $ fail $ "bad struct assignment from " ++ printTree value ++ " to " ++ printTree lhs
                                                       return lhs
inferExpr env (EAss lhs rhs) = do
                                 typlhs <- inferExpr env lhs
                                 typrhs <- inferExpr env rhs
                                 when (typlhs /= typrhs) $ fail $ "bad assignment from " ++ printTree rhs ++ " to " ++ printTree lhs ++ " (" ++ printTree typrhs ++ " to " ++ printTree typlhs ++ ")"
                                 return typlhs


checkTypeExists :: Env -> Type -> Err ()
checkTypeExists (_,_,structs) typ = case typ of
                                            TypeId ident -> case Map.lookup ident structs of
                                                              Nothing -> fail $ "bad datatype " ++ printTree ident
                                                              _ -> return ()
                                            _ -> return ()


checkStm :: Env -> Stm -> Err Env
checkStm env (SReturn e) = do
                             _ <- inferExpr env e
                              --TODO: check with function signature
                             return env
checkStm env (SExp e) = do
                          _ <- inferExpr env e
                          return env
checkStm env (SInit t i e) = do
                               checkTypeExists env t
                               checkExpType env t e
                               --TODO: check if ident exists already
                               return $ insertVarEnv env t i
checkStm env (SDecls t ids) = do
                                checkTypeExists env t
                                --TODO: check if ident exists already
                                return $ insertVarsEnv env t ids
checkStm env (SWhile e s) = do
                              conditionType <- inferExpr env e
                              when (conditionType /= Type_bool) $ fail $ "bad condition type " ++ printTree e ++ " in while: expected bool, found " ++ printTree conditionType
                              innerEnv <- envNewBlock env
                              innerEnv2 <- checkStm innerEnv s
                              env2 <- envPopBlock innerEnv2
                              return env2
checkStm env (SBlock ss) = do
                             innerEnv <- envNewBlock env
                             innerEnv2 <- checkStms innerEnv ss
                             env2 <- envPopBlock innerEnv2
                             return env2
checkStm env (SIfElse e s1 s2) = do
                                   conditionType <- inferExpr env e
                                   when (conditionType /= Type_bool) $ fail $ "bad condition type " ++ printTree e ++ " in if: expected bool, found " ++ printTree conditionType
                                   innerEnv1 <- envNewBlock env
                                   innerEnv2 <- checkStm innerEnv1 s1
                                   env2 <- envPopBlock innerEnv2
                                   innerEnv3 <- envNewBlock env2
                                   innerEnv4 <- checkStm innerEnv3 s2
                                   env3 <- envPopBlock innerEnv4
                                   return env3


envNewBlock :: Env -> Err Env
envNewBlock (sigs,c,structs) = return (sigs, [Map.fromList []] ++ c,structs)


envPopBlock :: Env -> Err Env
envPopBlock (sigs,c,structs) = return (sigs, tail c, structs)


checkStms :: Env -> [Stm] -> Err Env
checkStms env stms = foldM checkStm env stms


checkDef :: Env -> Def -> Err Env
checkDef env (DFun _ _ args stmts) = do
                                                 env2 <- envNewBlock env
                                                 env3 <- insertFunVars env2 args --TODO: check for unique arguments, e.g. "void foo(int i, double i){}"
                                                 env4 <- checkStms env3 stmts
                                                 --TODO: iterate over stmts if return has correct type
                                                 env5 <- envPopBlock env4
                                                 return env5

checkDef (sigs, ctxs, structs) (DStruct ident fields) = do
                                                          case Map.lookup ident structs of
                                                            Nothing -> return (sigs, ctxs, Map.insert ident f structs)
                                                                         where f = Map.fromList [(i,t) | (FDecl t i) <- fields]
                                                                         --TODO: check fields are name unique, e.g "struct s { int i; double i; };"
                                                            Just _ -> fail $ "struct " ++ printTree ident ++ " already defined"


checkDefs :: Env -> [Def] -> Err Env
checkDefs env defs = foldM checkDef env defs


newEnv :: Env
newEnv = (Map.fromList builtIns, [], Map.fromList [] )
    where builtIns = [(Id "printInt",([Type_int], Type_void)), (Id "printDouble",([Type_double], Type_void)), (Id "readInt",([], Type_int)), (Id "readDouble",([], Type_double))]


extendSigs :: Env -> Def -> Err Env
extendSigs (sigs,var,structs) (DFun returntype i args _) = do
                                                             case Map.lookup i sigs of
                                                               Nothing -> return (Map.insert i (argtypes,returntype) sigs,var,structs)
                                                                            where argtypes = [t | ADecl t _ <- args]
                                                               Just _ -> fail $ "function " ++ printTree i ++ " already defined"
extendSigs env (DStruct _ _) = return env


typecheck :: Program -> Err Env
typecheck (PDefs defs) = do
                           sigEnv <- foldM extendSigs newEnv defs
                           endEnv <- checkDefs sigEnv defs
                           return endEnv
