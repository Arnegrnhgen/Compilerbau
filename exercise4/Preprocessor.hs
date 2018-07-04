{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Preprocessor where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Control.Monad.State (State, StateT, MonadState, MonadIO, runStateT, execStateT, gets, modify, execState, evalState, lift, liftIO, liftM)
import Text.Regex (mkRegex, subRegex, matchRegex)
import ParCPP (myLexer)
import LexCPP (Token(..), Tok(..))
--import Filesystem.Path (append)


data PPState = PPState {
    definitions :: Map.Map String String ,
    --ifdefs      :: [String] ,
    result      :: String ,
    path        :: FilePath
    --TODO: map for macros, e.g. max
} deriving (Show,Read,Eq,Ord)


--newtype PPSTATE a = PPSTATE { runPPState :: State PPState a }
--  deriving (Functor, Applicative, Monad, MonadState PPState )


appendString :: String -> StateT PPState IO ()
appendString s = do
    str <- gets result
    modify $ \m -> m { result = str ++ s }


initPPState :: FilePath -> PPState
initPPState p = PPState Map.empty [] p


--evalPPState :: State PPState a -> State PPState
--evalPPState m = execState (runPPState m) initPPState


preprocess :: String -> FilePath -> IO String
preprocess s p = do
    let fileLines = lines s
    --error $ show $ map trim fileLines
    let state = execStateT (preprocessLines (map trim fileLines)) (initPPState p)
    (liftM result) state
    --return $ result $ evalPPState $ preprocessLines (map trim fileLines)


replaceM :: String -> (String, String) -> String
replaceM s (search,replace) = do
    let r = mkRegex ("(\\W|^)" ++ search ++ "(\\W|$)")
    subRegex r s ("\\1 " ++ replace ++ "\\2") --TODO remove space


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
preprocessLines (x:xs) = case head x of
        '#' -> do parseDirective x
                  preprocessLines xs
        _ -> do modifyNormalLine x
                appendString "\n"
                preprocessLines xs


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


parseDirective :: String -> StateT PPState IO ()
parseDirective dir
    | "#define " `isPrefixOf` dir = procDefine dir
    | "#include " `isPrefixOf` dir = procInclude dir
    | "#ifdef " `isPrefixOf` dir = procIfdef dir
    | "#endif " `isPrefixOf` dir = procEndif dir
    | otherwise = error $ "Invalid pp directive: " ++ dir


procDefine :: String -> StateT PPState IO ()
procDefine s = do
    defs <- gets definitions
    --REMARK: \s in [] does not work
    let r = mkRegex "#define\\s+([^(\\ \\t]+)((\\s+(.*))|$)"
    case matchRegex r s of
        Nothing -> error $ "Invalid1 #define: " ++ s
        Just x -> case length x of
            4 -> do rstr <- replaceStr (x !! 3)
                    modify $ \m -> m { definitions = Map.insert (head x) rstr defs }
                    return ()
            _ -> error $ "Invalid2 #define (" ++ show (length x) ++ "): " ++ show x
--TODO: macros, e.g. '#define max(a,b) a'


procIfdef :: String -> StateT PPState IO ()
procIfdef = undefined


procEndif :: String -> StateT PPState IO ()
procEndif = undefined


procInclude :: String -> StateT PPState IO ()
procInclude s = do
    let r = mkRegex "#include\\s+\"(.+)\"$"
    case matchRegex r s of
        Nothing -> error $ "Invalid1 #include: " ++ s
        Just x -> case length x of
            1 -> do
                p <- gets path
                --fileContent <- lift (readFile (append p (FilePath (head x)))) TODO append
                fileContent <- lift (readFile (head x))
                preprocessLines (lines fileContent)
            _ -> error $ "Invalid2 #include (" ++ show (length x) ++ "): " ++ show x
