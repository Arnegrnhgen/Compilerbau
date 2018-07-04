{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Preprocessor where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Control.Monad.State (StateT, execStateT, gets, modify, lift, liftM, foldM)
import Text.Regex (mkRegex, matchRegex)
import ParCPP (myLexer)
import LexCPP (Token(..), Tok(..))
import System.FilePath.Posix (FilePath, combine)


data PPState = PPState {
    definitions :: Map.Map String String ,
    ifdefs      :: [String] ,
    result      :: String ,
    path        :: FilePath
    --TODO: map for macros, e.g. max
} deriving (Show,Read,Eq,Ord)


appendString :: String -> StateT PPState IO ()
appendString s = do
    str <- gets result
    modify $ \m -> m { result = str ++ s }


initPPState :: FilePath -> PPState
initPPState p = PPState Map.empty [] [] p


preprocess :: String -> FilePath -> IO String
preprocess s p = do
    let fileLines = lines s
    let state = execStateT (preprocessLines (map trim fileLines)) (initPPState p)
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


modifyNormalLine :: String -> StateT PPState IO ()
modifyNormalLine s = do
    let tks = myLexer s
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
    defs <- gets definitions
    --REMARK: \s in [] does not work
    let r = mkRegex "#define\\s+(\\w+)((\\s+(.*))|$)"
    case matchRegex r s of
        Nothing -> error $ "Invalid1 #define: " ++ s
        Just x -> case length x of
            4 -> do rstr <- replaceStr (x !! 3)
                    modify $ \m -> m { definitions = Map.insert (head x) rstr defs }
                    return ()
            _ -> error $ "Invalid2 #define (" ++ show (length x) ++ "): " ++ show x
--TODO: macros, e.g. '#define max(a,b) a'


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
