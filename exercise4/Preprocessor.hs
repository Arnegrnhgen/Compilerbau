{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Preprocessor where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Control.Monad.State (StateT, execStateT, gets, modify, lift, liftM, foldM)
import Text.Regex (mkRegex, matchRegex, subRegex)
import ParCPP (myLexer)
import LexCPP (Token(..), Tok(..))
import System.FilePath.Posix (FilePath, combine)
import Data.List.Split (splitOn)


data Macro = Macro {
    params  :: [String] ,
    replace :: String
} deriving (Show,Read,Eq,Ord)

data PPState = PPState {
    definitions :: Map.Map String String ,
    ifdefs      :: [String] ,
    result      :: String ,
    path        :: FilePath ,
    macros      :: Map.Map String Macro
} deriving (Show,Read,Eq,Ord)


appendString :: String -> StateT PPState IO ()
appendString s = do
    str <- gets result
    modify $ \m -> m { result = str ++ s }


initPPState :: FilePath -> PPState
initPPState p = PPState Map.empty [] [] p Map.empty


preprocess :: String -> FilePath -> IO String
preprocess s p = do
    let fileLines = lines s
    let state = execStateT (preprocessLines (map trim fileLines)) (initPPState p)
    s1 <- state
    putStrLn (show s1)
    (liftM result) state


replaceStr :: String -> StateT PPState IO String
replaceStr s = do
    defs <- gets definitions
    case Map.lookup s defs of
        Nothing -> return s
        Just x -> return x


functk :: Tok -> StateT PPState IO ()
functk (TV ident) = replaceStr ident >>= appendString
functk (TI ident) = replaceStr ident >>= appendString
functk (TD ident) = replaceStr ident >>= appendString
functk (TL ident) = replaceStr ident >>= appendString
functk (TS ident _) = replaceStr ident >>= appendString
functk (TC ident) = replaceStr ident >>= appendString
functk (T_Id ident) = replaceStr ident >>= appendString


func :: Token -> StateT PPState IO ()
func (PT _ tk) = do
    functk tk
    appendString " "
func (Err _) = error "Lexer error"


searchAndReplace :: String -> (String, Macro) -> StateT PPState IO String
searchAndReplace s (macroname, macro) = do
    let r = mkRegex (macroname ++ "\\(([a-zA-Z0-9_,]*)\\)")


modifyNormalLine :: String -> StateT PPState IO ()
modifyNormalLine s = do
    ms <- gets macros
    s1 <- foldM searchAndReplace s (Map.toList ms)
    let tks = myLexer s1
    mapM_ func tks

preprocessLines :: [String] -> StateT PPState IO ()
preprocessLines [] = return ()
preprocessLines ([]:xs) = preprocessLines xs
preprocessLines (x:xs) = do
    parseConditional x
    sc <- shouldCompile
    if sc then case head x of
            '#' -> do parseDirective x
                      preprocessLines xs
            _ -> do modifyNormalLine x
                    appendString "\n"
                    preprocessLines xs
    else
        preprocessLines xs


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


parseConditional :: String -> StateT PPState IO ()
parseConditional dir
    | "#ifdef" `isPrefixOf` dir = procIfdef dir
    | "#endif" `isPrefixOf` dir = procEndif dir
    | otherwise = return ()
parseDirective :: String -> StateT PPState IO ()
parseDirective dir
    | "#define" `isPrefixOf` dir = procDefine dir
    | "#include" `isPrefixOf` dir = procInclude dir
    | otherwise = return ()


procDefine :: String -> StateT PPState IO ()
procDefine s = do
    --REMARK: \s in [] does not work
    let r = mkRegex "#define\\s+(\\w+)((\\s+(.*))|$)"
    case matchRegex r s of
        Nothing -> procDefineMacro s
        Just x -> case length x of
            4 -> do rstr <- replaceStr (x !! 3)
                    defs <- gets definitions
                    modify $ \m -> m { definitions = Map.insert (head x) rstr defs }
                    return ()
            _ -> error $ "Invalid1 #define (" ++ show (length x) ++ "): " ++ show x


procDefineMacro :: String -> StateT PPState IO ()
procDefineMacro s = do
    let r = mkRegex "#define\\s+(\\w+)\\(([a-zA-Z0-9_,]*)\\)\\s+(.+)$"
    case matchRegex r s of
        Nothing -> error $ "Invalid2 #define: " ++ s
        Just x -> case length x of
            3 -> do
                let name = x !! 0
                let rawparams = x !! 1
                let replaces = x !! 2
                let parameters = map trim $ splitOn "," rawparams

                ms <- gets macros
                modify $ \m -> m { macros = Map.insert name (Macro parameters replaces) ms }
                return ()

            _ -> error $ "Invalid3 #define (" ++ show (length x) ++ "): " ++ show x


replaceM :: String -> (String, String) -> String
replaceM s (search,replaces) = do
    let r = mkRegex ("(\\W|^)" ++ search ++ "(\\W|$)")
    subRegex r s ("\\1 " ++ replaces ++ "\\2") --TODO remove space


applyMacro :: Macro -> [String] -> String
applyMacro (Macro ps r) xs = foldl replaceM r (zip ps xs)


isDefined :: Bool -> String -> StateT PPState IO Bool
isDefined False _ = return False
isDefined True s = do
    defs <- gets definitions
    return $ Map.member s defs


shouldCompile :: StateT PPState IO Bool
shouldCompile = do
    st <- gets ifdefs
    foldM isDefined True st


procIfdef :: String -> StateT PPState IO ()
procIfdef s = do
    let r = mkRegex "#ifdef\\s+(\\w+)\\s*$"
    case matchRegex r s of
        Nothing -> error $ "Invalid1 #ifdef: " ++ s
        Just x -> case length x of
            1 -> do st <- gets ifdefs
                    modify $ \m -> m { ifdefs = (head x) : st }
            _ -> error $ "Invalid2 #ifdef (" ++ show (length x) ++ "): " ++ show x


procEndif :: String -> StateT PPState IO ()
procEndif _ = do
    st <- gets ifdefs
    case st of
        [] -> error "Invalid endif"
        _ -> modify $ \m -> m { ifdefs = tail st }


procInclude :: String -> StateT PPState IO ()
procInclude s = do
    let r = mkRegex "#include\\s+\"(.+)\"$"
    case matchRegex r s of
        Nothing -> error $ "Invalid1 #include: " ++ s
        Just x -> case length x of
            1 -> do
                p <- gets path
                fileContent <- lift $ readFile $ combine p $ head x
                preprocessLines (lines fileContent)
            _ -> error $ "Invalid2 #include (" ++ show (length x) ++ "): " ++ show x
