{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Preprocessor where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Control.Monad.State
import Text.Regex


data PPState = PPState {
    definitions :: [(String, String)] ,
    ifdefs      :: [String] ,
    result      :: String
    --TODO: map for macros, e.g. max
} deriving (Show,Read,Eq,Ord)

newtype PPSTATE a = PPSTATE { runPPState :: State PPState a }
  deriving (Functor, Applicative, Monad, MonadState PPState )



appendString :: String -> PPSTATE ()
appendString s = do
    str <- gets result
    modify $ \m -> m { result = str ++ s }


initPPState :: PPState
initPPState = PPState [] [] []


evalPPState :: PPSTATE a -> PPState
evalPPState m = execState (runPPState m) initPPState


preprocess :: String -> IO String
preprocess s = do
    let fileLines = lines s
    --error $ show $ map trim fileLines
    --return $ evalState (preprocess_lines (map trim fileLines)) initPPState
    return $ show $ evalPPState $ preprocess_lines (map trim fileLines)


replaceM :: String -> (String, String) -> String
replaceM s (search,replace)= do
    let r = mkRegex ("(\\W|^)" ++ search ++ "(\\W|$)")
    subRegex r s ("\\1 " ++ replace ++ "\\2") --TODO remove space


modifyNormalLine :: String -> PPSTATE String
modifyNormalLine s = do
    defs <- gets definitions
    return $ foldl replaceM s defs

preprocess_lines :: [String] -> PPSTATE ()
preprocess_lines [] = return ()
preprocess_lines ([]:xs) = preprocess_lines xs
preprocess_lines (x:xs) = do
    case head x of
        '#' -> do parseDirective x
                  preprocess_lines xs
        otherwise -> do line <- modifyNormalLine x
                        appendString line
                        rest <- preprocess_lines xs
                        return ()



trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


parseDirective :: String -> PPSTATE ()
parseDirective dir = do
    if "#define " `isPrefixOf` dir then addDefine dir else
        if "#include " `isPrefixOf` dir then includeFile dir else
            if "#ifdef " `isPrefixOf` dir then procIfdef dir else
                if "#endif " `isPrefixOf` dir then procEndif dir else
                    error $ "Invalid pp directive: " ++ dir


addDefine :: String -> PPSTATE ()
addDefine s = do
    defs <- gets definitions
    let r = mkRegex "#define\\s+([^(\\s]+)((\\s+(.*))|$)"
    case matchRegex r s of
        Nothing -> error $ "Invalid1 #define: " ++ s
        Just x -> case length x of
                    4 -> modify $ \s -> s { definitions = ((head x), (head $ tail $ tail $ tail x)) : defs }
                    otherwise -> error $ "Invalid2 #define (" ++ show (length x) ++ "): " ++ show x
--TODO: replace a with 3 in '#define b a'
--TODO: macros, e.g. '#define max(a,b) a'


procIfdef :: String -> PPSTATE ()
procIfdef = undefined


procEndif :: String -> PPSTATE ()
procEndif = undefined


includeFile :: String -> PPSTATE ()
includeFile = undefined
