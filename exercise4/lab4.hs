import System.Environment (getArgs)
import System.Exit (exitFailure)

import ParCPP (pProgram, myLexer)
import ErrM (Err(..))

import System.IO (hPutStrLn, stderr)

import TypeChecker (typecheck)
import Preprocessor (preprocess)
import Emit (codegen)


check :: String -> IO () 
check s = case pProgram (myLexer s) of
            Bad err  -> do hPutStrLn stderr "SYNTAX ERROR"
                           hPutStrLn stderr err
                           exitFailure 
            Ok  tree -> case typecheck tree of
                          Bad err -> do hPutStrLn stderr "TYPE ERROR"
                                        hPutStrLn stderr err
                                        exitFailure 
                          Ok (tree_a, structs) -> do
                                         code <- codegen tree_a structs
                                         putStrLn code


main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= preprocess >>= check
            _      -> do putStrLn "Usage: lab4 <SourceFile>"
                         exitFailure
