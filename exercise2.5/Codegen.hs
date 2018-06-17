{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

--import AbsCPP
--import ErrM
import qualified Data.Map as Map
import Control.Monad.State
import Data.List
import Data.Function

import qualified LLVM.AST as LAST
import qualified LLVM.AST.Global as LASTG
import qualified LLVM.AST.Linkage as LASTL
import qualified LLVM.AST.Constant as LASTC
import qualified LLVM.AST.CallingConvention as LASTCC
import qualified LLVM.AST.Attribute as LASTA
import qualified LLVM.AST.IntegerPredicate as LASTIP
import qualified LLVM.AST.FloatingPointPredicate as LASTFP


type Names = Map.Map String Int
type SymbolTable = [(String, LAST.Operand)]

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- IEEE 754 double
double :: LAST.Type
double = LAST.FloatingPointType 64 LAST.IEEE

int :: LAST.Type
int = LAST.IntegerType 32

void :: LAST.Type
void = LAST.VoidType

bool :: LAST.Type
bool = LAST.IntegerType 1

one :: LAST.Operand
one = cons $ LASTC.Int 32 1

zero :: LAST.Operand
zero = cons $ LASTC.Int 32 0

false :: LAST.Operand
false = zero

true :: LAST.Operand
true = one


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


newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )


newtype LLVM a = LLVM (State LAST.Module a)
  deriving (Functor, Applicative, Monad, MonadState LAST.Module )


sortBlocks :: [(LAST.Name, BlockState)] -> [(LAST.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))


createBlocks :: CodegenState -> [LASTG.BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)


makeBlock :: (LAST.Name, BlockState) -> LAST.BasicBlock
makeBlock (l, BlockState _ s t) = LAST.BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "CODEGEN ERROR: Block has no terminator: " ++ show l


runLLVM :: LAST.Module -> LLVM a -> LAST.Module
runLLVM modul (LLVM m) = execState m modul


emptyModule :: String -> LAST.Module
emptyModule label = LAST.defaultModule { LAST.moduleName = label }


entryBlockName :: String
entryBlockName = "entry"


emptyCodegen :: CodegenState
emptyCodegen = CodegenState (LAST.Name entryBlockName) Map.empty [] 1 0 Map.empty


execCodegen :: [(String, LAST.Operand)] -> Codegen a -> CodegenState
execCodegen vars m = execState (runCodegen m) emptyCodegen { symtab = vars }


addDefn :: LAST.Definition -> LLVM ()
addDefn d = do
  defs <- gets LAST.moduleDefinitions
  modify $ \s -> s { LAST.moduleDefinitions = defs ++ [d] }


define ::  LAST.Type -> String -> [(LAST.Type, LAST.Name)] -> [LAST.BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  LAST.GlobalDefinition $ LAST.functionDefaults {
    LASTG.name        = LAST.Name label
  , LASTG.parameters  = ([LAST.Parameter ty nm [] | (ty, nm) <- argtys], False)
  , LASTG.returnType  = retty
  , LASTG.basicBlocks = body
  }


external ::  LAST.Type -> String -> [(LAST.Type, LAST.Name)] -> LLVM ()
external retty label argtys = addDefn $
  LAST.GlobalDefinition $ LAST.functionDefaults {
    LASTG.name        = LAST.Name label
  , LASTG.linkage     = LASTL.External
  , LASTG.parameters  = ([LAST.Parameter ty nm [] | (ty, nm) <- argtys], False)
  , LASTG.returnType  = retty
  , LASTG.basicBlocks = []
  }


entry :: Codegen LAST.Name
entry = gets currentBlock


emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing


uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)


addBlock :: String -> Codegen LAST.Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (LAST.Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (LAST.Name qname)


setBlock :: LAST.Name -> Codegen LAST.Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname


getBlock :: Codegen LAST.Name
getBlock = gets currentBlock


modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }


current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "CODEGEN ERROR: No such block: " ++ show c


fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1


local ::  LAST.Name -> LAST.Operand
local = LAST.LocalReference int --TODO: add parameter for different types


externf :: LAST.Name -> LAST.Operand
externf = LAST.ConstantOperand . LASTC.GlobalReference int --TODO: add parameter for different types


assign :: String -> LAST.Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = (var, x) : lcls }


getvar :: String -> Codegen LAST.Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "CODEGEN ERROR: Local variable not in scope: " ++ show var


getsymtab :: Codegen SymbolTable
getsymtab = gets symtab


restoresymtab :: SymbolTable -> Codegen ()
restoresymtab t = modify $ \s -> s { symtab = t }

instr :: LAST.Instruction -> Codegen LAST.Operand
instr ins = do
  n <- fresh
  let ref = LAST.UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref LAST.:= ins) : i } )
  return $ local ref


terminator :: LAST.Named LAST.Terminator -> Codegen (LAST.Named LAST.Terminator)
terminator trm = do
  blk <- current
  case term blk of
    Nothing -> modifyBlock (blk { term = Just trm }) >> return trm
    Just x -> return x -- error $ "terminator already set"
  --return trm


--TODO: adopt for integer operations
fadd :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
fadd a b = instr $ LAST.FAdd LAST.NoFastMathFlags a b []
fsub :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
fsub a b = instr $ LAST.FSub LAST.NoFastMathFlags a b []
fmul :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
fmul a b = instr $ LAST.FMul LAST.NoFastMathFlags a b []
fdiv :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
fdiv a b = instr $ LAST.FDiv LAST.NoFastMathFlags a b []

iadd :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
iadd a b = instr $ LAST.Add True False a b []
isub :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
isub a b = instr $ LAST.Sub True False a b []
imul :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
imul a b = instr $ LAST.Mul True False a b []
idiv :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
idiv a b = instr $ LAST.SDiv False a b []


fcmp :: LASTFP.FloatingPointPredicate -> LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
fcmp cond a b = instr $ LAST.FCmp cond a b []


icmp :: LASTIP.IntegerPredicate -> LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
icmp cond a b = instr $ LAST.ICmp cond a b []


br :: LAST.Name -> Codegen (LAST.Named LAST.Terminator)
br val = terminator $ LAST.Do $ LAST.Br val []


cbr :: LAST.Operand -> LAST.Name -> LAST.Name -> Codegen (LAST.Named LAST.Terminator)
cbr cond tr fl = terminator $ LAST.Do $ LAST.CondBr cond tr fl []


ret :: LAST.Operand -> Codegen (LAST.Named LAST.Terminator)
ret val = terminator $ LAST.Do $ LAST.Ret (Just val) []


toArgs :: [LAST.Operand] -> [(LAST.Operand, [LASTA.ParameterAttribute])]
toArgs = map (\x -> (x, []))


call :: LAST.Operand -> [LAST.Operand] -> Codegen LAST.Operand
call fn args = instr $ LAST.Call Nothing LASTCC.C [] (Right fn) (toArgs args) [] []


alloca :: LAST.Type -> Codegen LAST.Operand
alloca ty = instr $ LAST.Alloca ty Nothing 0 []


store :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
store ptr val = instr $ LAST.Store False ptr val Nothing 0 []


load :: LAST.Operand -> Codegen LAST.Operand
load ptr = instr $ LAST.Load False ptr Nothing 0 []


cons :: LASTC.Constant -> LAST.Operand
cons = LAST.ConstantOperand
