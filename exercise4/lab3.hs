import System.Environment (getArgs)
import System.Exit (exitFailure)

--import AbsCPP
--import LexCPP
import ParCPP
import ErrM

import System.IO

import TypeChecker
--import Interpreter
import Codegen
import Emit

import qualified LLVM.AST as LAST
--import qualified LLVM.Module as LMOD

-- driver

initModule :: LAST.Module
initModule = emptyModule "codegen"


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
                                         code <- codegen initModule tree_a structs
                                         putStrLn code
                                         return ()


preprocess :: String -> IO String
preprocess = undefined


main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= preprocess >>= check
            _      -> do putStrLn "Usage: lab3 <SourceFile>"
                         exitFailure
