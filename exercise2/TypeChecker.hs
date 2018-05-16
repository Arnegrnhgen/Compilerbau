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
checkfunctypes ::  [String] -> [String] -> String
checkfunctypes [] types = ""
checkfunctypes (t:args) types = (if (elem t types ) then "" else "type not found: " ++ t ++ "\n") ++ checkfunctypes args types


  --foldl (++) "" ["type not found: " ++ x ++ "\n" |TypeId (Id x) <- [ x | x<- t:[t2 | ( ADecl t2 _ ) <- args ],not (x `elem` types)] ]




checkdef :: Env -> Def -> Err Env
checkdef (DFun t (Id "main") args _) (Con types _) =  (if (not (t == Type_int)) then "main() must return int\n"  else "") ++
                                                      (if (not (null args))     then "main() must not take any parameters\n" else "") ++
                                                      checkfunctypes [t2 | (ADecl (TypeId (Id t2)) _) <- (ADecl t (Id "")):args] (Map.keys types)


checkdef (DFun t _ args _) (Con types _)           =  checkfunctypes [t2 | (ADecl (TypeId (Id t2)) _) <- (ADecl t (Id "")):args] (Map.keys types)


--checkdef (DStruct _ fields) (Con types _) = checkfunctypes [t | (FDecl (TypeId (Id t)) _) <- fields] (Map.keys types)
checkdef (DStruct _ fields) (Con types _) = ""

regdef :: Def -> Context -> Context
regdef (DFun t (Id i) args _) (Con types m) = Con types (Map.insert i (t,args) m)
regdef (DStruct _ fields) context = context
--regdef (DStruct (Id i) fields ) (Con types m)= (Con (Map.insert i fields types) m)

checkdefs :: [Def] -> Context -> (String,Context)
checkdefs [] context = ("",context)
checkdefs (d:defs) context  = (checkdef d context ++ msg , context2)
                                where (msg,context2 ) = (checkdefs defs (regdef d context) )

   -}


{-
    TODO:
	inferExpr (page 72)
	finish checkstm
	    - eval expr
	    - extend context
	finish checkDef for structs
	add function parameters to new block context
	check function returntype
	inferUnaryExpr(++i,i++) - inferBinExpr(a+b)
	use printTree
	implement inferNumBin()
-}

lookupCtx :: [Context] -> Id -> Err Type
lookupCtx [] (Id str) = (Bad ("identifier " ++ str ++ " not found"))
lookupCtx (ctx:ctxs) ident = case (Map.lookup ident ctx) of
                                 Nothing -> lookupCtx ctxs ident
                                 Just x -> Ok x


lookupVarEnv :: Env -> Id -> Err Type
lookupVarEnv (sigs,ctxs,_) ident = lookupCtx ctxs ident

lookupSigs :: Sigs -> Id -> [Exp] -> Err ([Type], Type)
lookupSigs sigs (Id str) args = case (Map.lookup (Id str) sigs) of
                                    Nothing -> (Bad ("function identifier " ++ str ++ " not found"))
                                    Just x -> Ok x

lookupFunEnv :: Env -> Id -> [Exp] -> Err ([Type], Type)
lookupFunEnv (sigs,ctxs,_) ident args = lookupSigs sigs ident args

checkFunArgs :: Env -> [Exp] -> ([Type],Type) -> Err Type
checkFunArgs env exps (args,rettype) = foldM compareArgs rettype (zip exps args) --TODO: check same length
    where
                    compareArgs rtype (expr,t) = do
                                                    t2 <- inferExpr env expr
                                                    if t == t2 then Ok rtype else Bad ("function argument type mismatch: " ++ show (inferExpr env expr) ++ " vs " ++ show t)

insertVarEnv :: Env -> Type -> Id -> Env
insertVarEnv (sigs,[],structs) t i       = (sigs,([Map.fromList [(i,t)]]),structs)
insertVarEnv (sigs,(c:ctxs),structs) t i = (sigs,(Map.insert i t c) : ctxs,structs)

insertVarsEnv :: Env -> Type -> [Id] -> Env
insertVarsEnv env t [] = env
insertVarsEnv env t (i:is) = insertVarsEnv (insertVarEnv env t i) t is

insertFunVar :: Env -> Arg -> Env
insertFunVar (sigs,(c:ctxs),structs) (ADecl t i) = (sigs, (Map.insert i t c) : ctxs,structs)

insertFunVars :: Env -> [Arg] -> Err Env
insertFunVars env [] = Ok env
insertFunVars env (a:args) = insertFunVars (insertFunVar env a) args

lookUpStructField :: Env -> Id -> Type -> Err Type
lookUpStructField (sigs,ctxs,structs) membername (TypeId structname) = case Map.lookup structname structs of
                                                                 Nothing -> Bad ("struct " ++ show structname ++ " not found")
                                                                 Just struct -> case Map.lookup membername struct of
                                                                        Nothing -> Bad ("struct member " ++ show membername ++ " of struct " ++ show structname ++ " not found")
                                                                        Just field -> return field
lookUpStructField (sigs,ctxs,structs) membername structname = Bad (show structname ++ " not a struct")

checkExpType :: Env -> Type -> Exp -> Err Type
checkExpType env typ expr = do
                            typ2 <- inferExpr env expr
                            if typ2 == typ then
                              return typ
                            else
                              fail $ "type of " ++ printTree expr ++ " expected " ++ printTree typ ++ " but found " ++ printTree typ2

inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
inferBin types env exp1 exp2 = do
                                    typ <- inferExpr env exp1
                                    if elem typ types
                                       then
                                         checkExpType env typ exp2
                                       else
                                         fail $ "wrong type of expression " ++ printTree exp1

inferExpr :: Env -> Exp -> Err Type
inferExpr env ETrue = Ok Type_bool
inferExpr env EFalse = Ok Type_bool
inferExpr env (EInt _) = Ok Type_int
inferExpr env (EDouble _) = Ok Type_double
inferExpr env (EId ident) = lookupVarEnv env ident
inferExpr env (EApp ident exprs) = lookupFunEnv env ident exprs >>= checkFunArgs env exprs
inferExpr env (EProj (EId lhs) (EId rhs)) = lookupVarEnv env lhs >>= lookUpStructField env rhs
inferExpr env (EProj l r) = Bad ("invalid struct usage " ++ show l ++ "." ++ show r)
inferExpr env (EPIncr expr) = inferExpr env expr
inferExpr env (EPDecr expr) = inferExpr env expr
inferExpr env (EIncr expr) = inferExpr env expr
inferExpr env (EDecr expr) = inferExpr env expr
inferExpr env (ETimes exp1 exp2) = inferNumBin [Type_int, Type_double] env exp1 exp2
inferExpr env (EDiv exp1 exp2) = inferNumBin [Type_int, Type_double] env exp1 exp2
inferExpr env (EPlus exp1 exp2) = inferNumBin [Type_int, Type_double] env exp1 exp2
inferExpr env (EMinus exp1 exp2) = inferNumBin [Type_int, Type_double] env exp1 exp2
inferExpr env (ELt exp1 exp2) = inferNumBin [Type_int, Type_double] env exp1 exp2 >> return Type_bool
inferExpr env (EGt exp1 exp2) = inferNumBin [Type_int, Type_double] env exp1 exp2 >> return Type_bool
inferExpr env (ELtEq exp1 exp2) = inferNumBin [Type_int, Type_double] env exp1 exp2 >> return Type_bool
inferExpr env (EGtWq exp1 exp2) = inferNumBin [Type_int, Type_double] env exp1 exp2 >> return Type_bool
inferExpr env (EEq exp1 exp2) = inferBin [Type_int, Type_double, Type_bool] env exp1 exp2 >> return Type_bool
inferExpr env (ENEq exp1 exp2) = inferBin [Type_int, Type_double, Type_bool] env exp1 exp2 >> return Type_bool
inferExpr env (EAnd exp1 exp2) = inferBin [Type_bool] env exp1 exp2 >> return Type_bool
inferExpr env (EOr exp1 exp2) = inferBin [Type_bool] env exp1 exp2 >> return Type_bool
inferExpr env (EPrAss structname membername value) = do
                                                        lhs <- inferExpr env (EProj structname membername)
                                                        rhs <- inferExpr env value
                                                        if lhs == rhs then return lhs else Bad ("bad struct assignment from " ++ show value ++ " to " ++ show lhs)
inferExpr env (EAss lhs rhs) = do
                                lhs <- inferExpr env lhs
                                rhs <- inferExpr env rhs
                                if lhs == rhs then return lhs else Bad ("bad assignment from " ++ show rhs ++ " to " ++ show lhs)

checkStm :: Env -> Stm -> Err Env
checkStm env (SReturn e) = Ok env --TODO
checkStm env (SExp e) = case (inferExpr env e) of
                             Bad msg -> Bad msg
                             Ok _ -> Ok env
checkStm env (SInit t i e) = Ok (insertVarEnv env t i) -- TODO: check e:type == t and if t exists
checkStm env (SDecls t ids) = Ok (insertVarsEnv env t ids) --TODO: check if t exists
checkStm env (SWhile e s)
                       | inferExpr env e /= Ok Type_bool = Bad ("condition " ++ show e ++ " in while: expected bool, found " ++ show (inferExpr env e))
                       | otherwise = Ok env --TODO: check inner block
checkStm env (SBlock ss) = Ok env --TODO
checkStm env (SIfElse e s1 s2) = Ok env --TODO

envNewBlock :: Env -> Err Env
envNewBlock (sigs,c,structs) = Ok (sigs, [Map.fromList []] ++ c,structs)

envPopBlock :: Env -> Err Env
envPopBlock (sigs,c,structs) = Ok (sigs, tail c, structs)

foldStm :: Err Env -> [Stm] -> Err Env
foldStm (Bad msg) _ = Bad msg
foldStm (Ok env) [] = Ok env
foldStm (Ok env) (s:stms) = foldStm (checkStm env s) stms

checkStms :: Env -> [Stm] -> Err Env
checkStms env stms = foldStm (Ok env) stms --TODO foldM

checkDef :: Env -> Def -> Err Env
checkDef env (DFun rettype name args stmts) = do
                                                 env2 <- envNewBlock env
                                                 env3 <- insertFunVars env2 args
                                                 env4 <- checkStms env3 stmts
                                                 --TODO: iterate over stmts if return has correct type
                                                 env5 <- envPopBlock env4
                                                 return env5

checkDef (sigs, ctxs, structs) (DStruct ident fields) = return (sigs, ctxs, Map.insert ident f structs)
    where f = Map.fromList [(i,t) | (FDecl t i) <- fields]


checkDefs :: Err Env -> [Def] -> Err Env
checkDefs (Bad msg) _   = Bad msg
checkDefs (Ok env) defs = foldM checkDef env defs

newEnv :: Env
newEnv = (Map.fromList builtIns, [], Map.fromList [] )
    where builtIns = [(Id "printInt",([Type_int], Type_void)), (Id "printDouble",([Type_double], Type_void)), (Id "readInt",([], Type_int)), (Id "readDouble",([], Type_double))]



extendSigs :: Env -> Def -> Err Env -- TODO: failure cases (duplicate definition, ...)
extendSigs (sigs,var,structs) (DFun returntype i args _) = return (Map.insert i (argtypes,returntype) sigs,var,structs)
                                        where argtypes = [t | ADecl t _ <- args]
extendSigs env (DStruct _ _) = return env

typecheck :: Program -> Err Env
typecheck (PDefs defs) = checkDefs (foldM extendSigs newEnv defs) defs
