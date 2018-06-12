module Codegen where

import AbsCPP
import ErrM
import qualified Data.Map as Map
import qualified LLVM.AST as LAST


type Names = Map.Map String Int
type SymbolTable = [(String, LAST.Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: LAST.Name                     -- Name of the active block to append to
  , blocks       :: Map.Map LAST.Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable                   -- Function scope symbol table
  , blockCount   :: Int                           -- Count of basic blocks
  , count        :: Word                          -- Count of unnamed instructions
  , names        :: Names                         -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                                      -- Block index
  , stack :: [LAST.Named LAST.Instruction]            -- Stack of instructions
  , term  :: Maybe (LAST.Named LAST.Terminator)       -- Block terminator
} deriving Show


codegen :: Program -> Err ()
codegen _ = return ()
