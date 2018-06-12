import System.Environment (getArgs)
import System.Exit (exitFailure)

--import AbsCPP
--import LexCPP
import ParCPP
import ErrM

import TypeChecker
--import Interpreter
import Codegen

-- driver

check :: String -> IO () 
check s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
            Ok  tree -> case typecheck tree of
                          Bad err -> do putStrLn "TYPE ERROR"
                                        putStrLn err
                                        exitFailure 
                          Ok tree_a -> case codegen tree_a of
                                         Bad err -> do putStrLn "CODEGEN ERROR"
                                                       putStrLn err
                                                       exitFailure
                                         Ok _    -> putStrLn "OK"

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= check
            _      -> do putStrLn "Usage: lab3 <SourceFile>"
                         exitFailure
