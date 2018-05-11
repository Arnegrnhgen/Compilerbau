module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import qualified Data.Map as Map

--data Context = Con (Map.Map String [Field] )   (Map.Map String  (Type, [Arg]) ) deriving (Show)
type Env = (Sigs, [Context])
type Sigs = Map.Map Id ([Type],Type)
type Context = Map.Map Id Type
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
-}
{-     data Stm
    = SExp Exp
    | SDecls Type [Id]
    | SInit Type Id Exp
    | SReturn Exp
    | SWhile Exp Stm
    | SBlock [Stm]
    | SIfElse Exp Stm Stm  -}
checkStm :: Env -> Stm -> Err Env
checkStm env (SReturn e) = Ok env

envNewBlock :: Env -> Env
envNewBlock (sigs,c) = (sigs, c ++ [Map.fromList []])

envPopBlock :: Env -> Env
envPopBlock (sigs,c) = (sigs, init c) -- init() removes last element

foldStm :: Err Env -> [Stm] -> Err Env
foldStm (Bad msg) _ = Bad msg
foldStm (Ok env) [] = Ok env
foldStm (Ok env) (s:stms) = foldStm (checkStm env s) stms

checkDef :: Env -> Def -> Err Env
checkDef env (DFun rettype name args stmts) = do
                                                 -- check rettype
                                                 let env2 = envNewBlock env
                                                 -- add function parameters to block context
                                                 foldStm (Ok env2) stmts
                                                 let env3 = envPopBlock env2
                                                 return env3

newEnv :: Env
newEnv = (Map.fromList builtIns, [] )
    where builtIns = [(Id "printInt",([Type_int], Type_void)), (Id "printDouble",([Type_double], Type_void)), (Id "readInt",([], Type_int)), (Id "readDouble",([], Type_double))]



extendSigs :: Env -> Def -> Err Env -- TODO: failure cases (duplicate definition, ...)
extendSigs (sigs,var) (DFun returntype i args _) = return (Map.insert i (argtypes,returntype) sigs,var)
                                        where argtypes = [t | ADecl t _ <- args]
extendSigs env (DStruct _ _) = return env -- TODO
extendSigs env _ = return env

foldEnv :: Err Env -> [Def] -> Err Env
foldEnv (Bad msg) _ = Bad msg
foldEnv (Ok env) [] = Ok env
foldEnv (Ok env) (d:defs) = foldEnv (extendSigs env d) defs


typecheck :: Program -> Err Env
typecheck (PDefs defs) = do
                            env <- foldEnv (Ok newEnv) defs -- create symbol table (sigs), including built-ins
                            --env <- foldDefs
                            return env
