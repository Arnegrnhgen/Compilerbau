module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import qualified Data.Map as Map

data Context = Con [Type] (Map.Map String  (Type, [Arg]) ) deriving (Show)


add2context :: (Map.Map String  (Type, [Arg]) ) -> String -> (Type, [Arg]) -> (Map.Map String  (Type, [Arg]) )
add2context m name t = Map.insert name t m

checkdef :: Def -> Context -> (Bool,Context,String)
checkdef (DFun t (Id i) args _) (Con types m)= (elem t types ,Con types (Map.insert i (t,args) m) , "Type not found: '"++show t++"'")

checkdef (DStruct i _ ) (Con types m)= (True, Con (types ++ [TypeId i]) m,"")



checkdefs :: [Def] -> Context -> Err ()
checkdefs [] _ = return ()
checkdefs (d:defs) types  = if b
                             then (checkdefs defs types2)
                             else fail (show types2)
                              where
                                (b, types2, msg) = checkdef d types

typecheck :: Program -> Err ()

typecheck (PDefs defs) = checkdefs defs (Con [Type_bool, Type_int, Type_double, Type_void] (Map.fromList []) )


--(foldl (++) "" ["."| a <- (p . PDefs) ])
