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


checkdef :: Def -> Context -> String
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
newenv = (Map.fromList [],[] )

extend :: Env -> Def -> Err Env
extend (sigs,var) (DFun returntype i args _) = return (Map.insert i (argtypes,returntype) sigs,var)
                                        where argtypes = [t | ADecl t _ <- args]
extend env _= return env

extendbi :: Err Env -> Def -> Err Env
extendbi (Ok env) d = extend env d
extendbi (Bad s) d = (Bad s)


typecheck :: Program -> Err Env
typecheck (PDefs defs) = return ( foldl extendbi (Ok newenv) defs)

--typecheck (PDefs defs) = foldl (\env def ->extend env def) env defs

--typecheck (PDefs defs) = case checkdefs defs (Env (Map.fromList [],[] )) of
--                              ("", env) -> return ()
--                              (msg, env) -> fail (msg ++ (show context))


--(foldl (++) "" ["."| a <- (p . PDefs) ])
