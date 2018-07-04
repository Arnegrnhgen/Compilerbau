{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Preprocessor where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Control.Monad.State (State, MonadState, gets, modify, execState)
import Text.Regex (mkRegex, subRegex, matchRegex)
import ParCPP (myLexer)
import LexCPP (Token(..), Tok(..))


data PPState = PPState {
    definitions :: Map.Map String String ,
    --ifdefs      :: [String] ,
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
initPPState = PPState Map.empty []


evalPPState :: PPSTATE a -> PPState
evalPPState m = execState (runPPState m) initPPState


preprocess :: String -> IO String
preprocess s = do
    let fileLines = lines s
    --error $ show $ map trim fileLines
    --return $ evalState (preprocess_lines (map trim fileLines)) initPPState
    return $ result $ evalPPState $ preprocessLines (map trim fileLines)


replaceM :: String -> (String, String) -> String
replaceM s (search,replace) = do
    let r = mkRegex ("(\\W|^)" ++ search ++ "(\\W|$)")
    subRegex r s ("\\1 " ++ replace ++ "\\2") --TODO remove space


replaceStr :: String -> PPSTATE String
replaceStr s = do
    defs <- gets definitions
    case Map.lookup s defs of
        Nothing -> return s
        Just x -> return x


functk :: Tok -> PPSTATE ()
functk (TV ident) = replaceStr ident >>= appendString
functk (TI ident) = replaceStr ident >>= appendString
functk (TD ident) = replaceStr ident >>= appendString
functk (TL ident) = replaceStr ident >>= appendString
functk (TS ident _) = replaceStr ident >>= appendString
functk (TC ident) = replaceStr ident >>= appendString
functk (T_Id ident) = replaceStr ident >>= appendString


func :: Token -> PPSTATE ()
func (PT _ tk) = do
    functk tk
    appendString " "
func (Err _) = error "Lexer error"


modifyNormalLine :: String -> PPSTATE ()
modifyNormalLine s = do
    let tks = myLexer s
    mapM_ func tks


preprocessLines :: [String] -> PPSTATE ()
preprocessLines [] = return ()
preprocessLines ([]:xs) = preprocessLines xs
preprocessLines (x:xs) = case head x of
        '#' -> do parseDirective x
                  preprocessLines xs
        _ -> do modifyNormalLine x
                appendString "\n"
                preprocessLines xs


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


parseDirective :: String -> PPSTATE ()
parseDirective dir
    | "#define " `isPrefixOf` dir = procDefine dir
    | "#include " `isPrefixOf` dir = procInclude dir
    | "#ifdef " `isPrefixOf` dir = procIfdef dir
    | "#endif " `isPrefixOf` dir = procEndif dir
    | otherwise = error $ "Invalid pp directive: " ++ dir


procDefine :: String -> PPSTATE ()
procDefine s = do
    defs <- gets definitions
    --REMARK: \s in [] does not work
    let r = mkRegex "#define\\s+([^(\\ \\t]+)((\\s+(.*))|$)"
    case matchRegex r s of
        Nothing -> error $ "Invalid1 #define: " ++ s
        Just x -> case length x of
                    4 -> do
                        rstr <- replaceStr (x !! 3)
                        modify $ \m -> m { definitions = Map.insert (head x) rstr defs }
                    _ -> error $ "Invalid2 #define (" ++ show (length x) ++ "): " ++ show x
--TODO: macros, e.g. '#define max(a,b) a'


procIfdef :: String -> PPSTATE ()
procIfdef = undefined


procEndif :: String -> PPSTATE ()
procEndif = undefined


procInclude :: String -> PPSTATE ()
procInclude = undefined
