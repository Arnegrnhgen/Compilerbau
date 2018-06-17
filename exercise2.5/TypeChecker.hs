module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import qualified Data.Map as Map
import Control.Monad.State


data Env = Env {
    signatures :: Sigs ,
    contexts :: [Context],
    structures :: Structs } deriving (Show,Read,Eq,Ord)
type Sigs = Map.Map Id ([Type],Type)
type Context = Map.Map Id Type
type Structs = Map.Map Id Struct
type Struct = Map.Map Id Type


newEnv :: Env
newEnv = Env (Map.fromList builtIns) [] Map.empty
    where builtIns = [(Id "printInt", ([Type_int], Type_void)), (Id "printDouble", ([Type_double], Type_void)), (Id "readInt", ([], Type_int)), (Id "readDouble", ([], Type_double))]


typeExists :: Type -> StateT Env Err()
typeExists (TypeId ident) = do
                              structs <- gets structures
                              case Map.lookup ident structs of
                                Nothing -> fail $ "unknown datatype " ++ printTree ident
                                _ -> return ()
typeExists _ = return ()


initStructField :: Struct -> Field -> StateT Env Err Struct
initStructField s (FDecl typ ident) = do
                                        typeExists typ
                                        case Map.insertLookupWithKey (\_ a _ -> a) ident typ s of
                                          (Nothing, m2) -> return m2
                                          (Just _, _) -> fail $ "struct member " ++ printTree ident ++ " already defined"


parseSigs :: [Def] -> StateT Env Err ()
parseSigs [] = return ()
parseSigs (d:ds) = do
                     case d of
                       (DFun rettype ident args _) -> do
                                                        typeExists rettype
                                                        mapM_ typeExists argtypes
                                                        sigs <- gets signatures
                                                        case Map.insertLookupWithKey (\_ a _ -> a) ident (argtypes, rettype) sigs of
                                                          (Nothing, sigs2) -> modify $ \s -> s { signatures = sigs2 }
                                                          (Just _, _) -> fail $ "function " ++ printTree ident ++ " already defined"
                                                          where argtypes = [t | ADecl t _ <- args]
                       (DStruct ident fields) -> do
                                                   structs <- gets structures
                                                   case Map.lookup ident structs of
                                                     Nothing -> do
                                                                  f <- foldM initStructField Map.empty fields
                                                                  modify $ \s -> s { structures = Map.insert ident f structs }
                                                     Just _ -> fail $ "struct " ++ printTree ident ++ " already defined"
                     parseSigs ds


insertVar :: Type -> Id -> StateT Env Err ()
insertVar typ ident = do
                        (c:ctxs) <- gets contexts
                        case Map.insertLookupWithKey (\_ a _ -> a) ident typ c of
                          (Nothing, c2) -> modify $ \s -> s { contexts = c2 : ctxs }
                          (Just _, _) -> fail $ "variable " ++ printTree ident ++ " already defined"
                        return ()


insertFunParam :: Arg -> StateT Env Err ()
insertFunParam (ADecl typ ident) = insertVar typ ident


newBlock :: StateT Env Err ()
newBlock = do
             ctxs <- gets contexts
             modify $ \s -> s { contexts = Map.empty : ctxs }
             return ()


popBlock :: StateT Env Err ()
popBlock = do
             ctxs <- gets contexts
             modify $ \s -> s { contexts = tail ctxs }
             return ()


lookupVar :: Id -> StateT Env Err Type
lookupVar ident = do
                    ctxs <- gets contexts
                    lookupVarHelper ctxs
                    where
                        lookupVarHelper [] = fail $ "identifier " ++ printTree ident ++ " not found"
                        lookupVarHelper (c:cs) = case Map.lookup ident c of
                                                         Nothing -> lookupVarHelper cs
                                                         Just x -> return x


lookupFun :: Id -> StateT Env Err ([Type], Type)
lookupFun ident = do
                    sigs <- gets signatures
                    case Map.lookup ident sigs of
                      Nothing -> fail $ "function identifier " ++ printTree ident ++ " not found"
                      Just x -> return x


compareArgs :: Integer -> (Exp, Type) -> StateT Env Err Integer
compareArgs i (expr, t) = do
                            (t2, _) <- inferExpr expr
                            when (t /= t2) $ fail $ "function argument number " ++ show i ++ " type mismatch: expected " ++ printTree t2 ++ ", got " ++ printTree t
                            return (i+1)


checkFunArgs :: [Exp] -> [Type] -> StateT Env Err ()
checkFunArgs exps args = do
                           when (length exps /= length args) $ fail $ "bad number of function arguments: expected " ++ show (length args) ++ ", got " ++ show (length exps)
                           foldM_ compareArgs 1 (zip exps args)
                           return ()


lookUpStructField :: Id -> Type -> StateT Env Err Type
lookUpStructField membername (TypeId structname) = do
                                                     structs <- gets structures
                                                     case Map.lookup structname structs of
                                                       Nothing -> fail $ "struct " ++ show structname ++ " not found"
                                                       Just struct -> case Map.lookup membername struct of
                                                                        Nothing -> fail $ "struct member " ++ show membername ++ " of struct " ++ show structname ++ " not found"
                                                                        Just field -> return field
lookUpStructField _ structname = fail $ "identifier " ++ printTree structname ++ " is not a struct name"


isNumeric :: Type -> StateT Env Err ()
isNumeric Type_int = return ()
isNumeric Type_double = return ()
isNumeric t = fail $ "invalid numeric type " ++ printTree t


typesCompatible :: Type -> Type -> StateT Env Err Type
typesCompatible Type_int Type_int = return Type_int
typesCompatible Type_int Type_double = return Type_double
typesCompatible Type_double Type_int = return Type_double
typesCompatible Type_double Type_double = return Type_double
typesCompatible Type_bool Type_bool = return Type_bool
typesCompatible t1 t2 = fail $ "incompatible types " ++ printTree t1 ++ " and " ++ printTree t2


inferNumBin :: Exp -> Exp -> StateT Env Err (Type, Exp, Exp)
inferNumBin exp1 exp2 = do
                          (type1, exp1_a) <- inferExpr exp1
                          (type2, exp2_a) <- inferExpr exp2
                          isNumeric type1
                          isNumeric type2
                          typ' <- typesCompatible type1 type2
                          return (typ', exp1_a, exp2_a)


inferBin :: [Type] -> Exp -> Exp -> StateT Env Err (Type, Exp, Exp)
inferBin types exp1 exp2 = do
                                 (typ1, exp1_a) <- inferExpr exp1
                                 (typ2, exp2_a) <- inferExpr exp2
                                 unless (typ1 `elem` types) $ fail $ "wrong type of expression " ++ printTree exp1 ++ " for binary operation: " ++ printTree typ1
                                 unless (typ2 `elem` types) $ fail $ "wrong type of expression " ++ printTree exp2 ++ " for binary operation: " ++ printTree typ2
                                 typ' <- typesCompatible typ1 typ2
                                 return (typ', exp1_a, exp2_a)


inferExpr :: Exp -> StateT Env Err (Type, Exp)
inferExpr ETrue = return (Type_bool, ETyped ETrue Type_bool)
inferExpr EFalse = return (Type_bool, ETyped EFalse Type_bool)
inferExpr (EInt i) = return (Type_int, ETyped (EInt i) Type_int)
inferExpr (EDouble d) = return (Type_double, ETyped (EDouble d) Type_double)
inferExpr (EId ident) = do
                          typ <- lookupVar ident
                          return (typ, ETyped (EId ident) typ)
inferExpr (EApp ident exprs) = do
                                 (fargtypes, frettype) <- lookupFun ident
                                 checkFunArgs exprs fargtypes
                                 exprs_full <- mapM inferExpr exprs
                                 exprs_a <- mapM (\(_, e') -> return e') exprs_full
                                 return (frettype, ETyped (EApp ident exprs_a) frettype)
inferExpr (EProj lhs (EId rhs)) = do
                                    (lhstyp, lhs_a) <- inferExpr lhs
                                    typ' <- lookUpStructField rhs lhstyp
                                    return (typ', ETyped (EProj lhs_a (EId rhs)) typ')
inferExpr (EProj l r) = fail $ "invalid struct usage: " ++ printTree l ++ "." ++ printTree r
inferExpr (EPIncr expr) = do
                            (typ, expr_a) <- inferExpr expr
                            isNumeric typ
                            return (typ, ETyped (EPIncr expr_a) typ)
inferExpr (EPDecr expr) = do
                            (typ, expr_a) <- inferExpr expr
                            isNumeric typ
                            return (typ, ETyped (EPDecr expr_a) typ)
inferExpr (EIncr expr) = do
                           (typ, expr_a) <- inferExpr expr
                           isNumeric typ
                           return (typ, ETyped (EIncr expr_a) typ)
inferExpr (EDecr expr) = do
                           (typ, expr_a) <- inferExpr expr
                           isNumeric typ
                           return (typ, ETyped (EDecr expr_a) typ)
inferExpr (ETimes exp1 exp2) = do
                                 (typ, exp1_a, exp2_a) <- inferNumBin exp1 exp2
                                 return (typ, ETyped (ETimes exp1_a exp2_a) typ)
inferExpr (EDiv exp1 exp2) = do
                               (typ, exp1_a ,exp2_a) <- inferNumBin exp1 exp2
                               return (typ, ETyped (EDiv exp1_a exp2_a) typ)
inferExpr (EPlus exp1 exp2) = do
                                (typ, exp1_a, exp2_a) <- inferNumBin exp1 exp2
                                return (typ, ETyped (EPlus exp1_a exp2_a) typ)
inferExpr (EMinus exp1 exp2) = do
                                 (typ, exp1_a, exp2_a) <- inferNumBin exp1 exp2
                                 return (typ, ETyped (EMinus exp1_a exp2_a) typ)
inferExpr (ELt exp1 exp2) = do
                              (typ, exp1_a, exp2_a) <- inferNumBin exp1 exp2
                              return (Type_bool, ETyped (ELt exp1_a exp2_a) typ)
inferExpr (EGt exp1 exp2) = do
                              (typ, exp1_a, exp2_a) <- inferNumBin exp1 exp2
                              return (Type_bool, ETyped (EGt exp1_a exp2_a) typ)
inferExpr (ELtEq exp1 exp2) = do
                                (typ, exp1_a, exp2_a) <- inferNumBin exp1 exp2
                                return (Type_bool, ETyped (ELtEq exp1_a exp2_a) typ)
inferExpr (EGtWq exp1 exp2) = do
                                (typ, exp1_a, exp2_a) <- inferNumBin exp1 exp2
                                return (Type_bool, ETyped (EGtWq exp1_a exp2_a) typ)
inferExpr (EEq exp1 exp2) = do
                              (typ, exp1_a, exp2_a) <- inferBin [Type_int, Type_double, Type_bool] exp1 exp2
                              return (Type_bool, ETyped (EEq exp1_a exp2_a) typ)
inferExpr (ENEq exp1 exp2) = do
                               (typ, exp1_a, exp2_a) <- inferBin [Type_int, Type_double, Type_bool] exp1 exp2
                               return (Type_bool, ETyped (ENEq exp1_a exp2_a) typ)
inferExpr (EAnd exp1 exp2) = do
                               (typ, exp1_a, exp2_a) <- inferBin [Type_int, Type_double, Type_bool] exp1 exp2
                               return (Type_bool, ETyped (EAnd exp1_a exp2_a) typ)
inferExpr (EOr exp1 exp2) = do
                              (typ, exp1_a, exp2_a) <- inferBin [Type_int, Type_double, Type_bool] exp1 exp2
                              return (Type_bool, ETyped (EOr exp1_a exp2_a) typ)
inferExpr (EPrAss structname membername value) = do
                                                   (lhstyp, _) <- inferExpr (EProj structname membername)
                                                   (rhstyp, rhs_a) <- inferExpr value
                                                   when (lhstyp /= rhstyp) $ fail $ "bad struct assignment from " ++ printTree value ++ " to " ++ printTree structname ++ "." ++ printTree membername ++ " (" ++ printTree rhstyp ++ " to " ++ printTree lhstyp ++ ")"
                                                   return (lhstyp, ETyped (EPrAss structname membername rhs_a) lhstyp)
inferExpr (EAss lhs rhs) = do
                             (typlhs, lhs_a) <- inferExpr lhs
                             (typrhs, rhs_a) <- inferExpr rhs
                             when (typlhs /= typrhs) $ fail $ "bad assignment from " ++ printTree rhs ++ " to " ++ printTree lhs ++ " (" ++ printTree typrhs ++ " to " ++ printTree typlhs ++ ")"
                             return (typlhs, ETyped (EAss lhs_a rhs_a) typlhs)
inferExpr (ETyped _ _) = fail "invalid ETyped found in AST"


checkStm :: Stm -> StateT Env Err Stm
checkStm (SReturn e) = do
                         typ_expected <- lookupVar (Id "__return")
                         (typ_actual, e_a) <- inferExpr e
                         when (typ_expected /= typ_actual) $ fail $ "type of expression " ++ printTree e ++ " expected to be " ++ printTree typ_expected ++ ", but found " ++ printTree typ_actual
                         return (SReturn e_a)
checkStm (SDecls t ids) = do
                            typeExists t
                            mapM_ (insertVar t) ids
                            return (SDecls t ids)
checkStm (SExp e) = do
                      (_, e_a) <- inferExpr e
                      return (SExp e_a)
checkStm (SInit t i e) = do
                           typeExists t
                           (typ_actual, e_a) <- inferExpr e
                           when (t /= typ_actual) $ fail $ "type of expression " ++ printTree e ++ " expected to be " ++ printTree t ++ ", but found " ++ printTree typ_actual
                           insertVar t i
                           return (SInit t i e_a)
checkStm (SWhile e s) = do
                          (conditionType, e_a) <- inferExpr e
                          when (conditionType /= Type_bool) $ fail $ "bad condition type " ++ printTree e ++ " in while: expected bool, found " ++ printTree conditionType
                          s_a <- checkStm s
                          return (SWhile e_a s_a)
checkStm (SIfElse e s1 s2) = do
                               (conditionType, e_a) <- inferExpr e
                               when (conditionType /= Type_bool) $ fail $ "bad condition type " ++ printTree e ++ " in if: expected bool, found " ++ printTree conditionType
                               s1_a <- checkStm s1
                               s2_a <- checkStm s2
                               return (SIfElse e_a s1_a s2_a)
checkStm (SBlock ss) = do
                         newBlock
                         ss_a <- checkStmts ss
                         popBlock
                         return (SBlock ss_a)


checkStmts :: [Stm] -> StateT Env Err [Stm]
checkStmts = mapM checkStm


checkDefs :: [Def] -> StateT Env Err [Def]
checkDefs = mapM checkDef


checkDef :: Def -> StateT Env Err Def
checkDef (DFun rettype i args stmts) = do
                                         newBlock
                                         insertVar rettype (Id "__return")
                                         mapM_ insertFunParam args
                                         stmts_a <- checkStmts stmts
                                         popBlock
                                         return (DFun rettype i args stmts_a)
checkDef d@(DStruct _ _) = return d


typecheck :: Program -> Err Program
typecheck (PDefs defs) = do
                           sigEnv <- execStateT (parseSigs defs) newEnv
                           adefs <- evalStateT (checkDefs defs) sigEnv
                           return (PDefs adefs)
