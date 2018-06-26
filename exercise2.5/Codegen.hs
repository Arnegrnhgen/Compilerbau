{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

--import AbsCPP
--import ErrM
import TypeChecker
import qualified Data.Map as Map
import Control.Monad.State
import Data.List
import Data.Function

import qualified LLVM.AST as LAST
import qualified LLVM.AST.Float as LASTF
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

intOne :: LAST.Operand
intOne = cons $ LASTC.Int 32 1

intZero :: LAST.Operand
intZero = cons $ LASTC.Int 32 0

doubleOne :: LAST.Operand
doubleOne = cons $ LASTC.Float (LASTF.Double 1)

doubleZero :: LAST.Operand
doubleZero = cons $ LASTC.Float (LASTF.Double 0)

false :: LAST.Operand
false = cons $ LASTC.Int 1 0

true :: LAST.Operand
true = cons $ LASTC.Int 1 1

--voidValue :: LAST.Operand
--voidValue = cons $ LAST.VoidType

data CodegenState
  = CodegenState {
    currentBlock :: LAST.Name                     -- Name of the active block to append to
  , blocks       :: Map.Map LAST.Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable                   -- Function scope symbol table
  , blockCount   :: Int                           -- Count of basic blocks
  , count        :: Word                          -- Count of unnamed instructions
  , names        :: Names                         -- Name Supply
  , structsRO    :: Structs
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
emptyCodegen = CodegenState (LAST.Name entryBlockName) Map.empty [] 1 0 Map.empty Map.empty


execCodegen :: [(String, LAST.Operand)] -> Structs -> Codegen a -> CodegenState
execCodegen vars structs m = execState (runCodegen m) emptyCodegen { symtab = vars, structsRO = structs }


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


structDefine :: String -> [LAST.Type] -> LLVM ()
structDefine name types = addDefn $
  LAST.TypeDefinition (LAST.Name name) (Just (LAST.StructureType False types))


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


getStructs :: Codegen Structs
getStructs = gets structsRO


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


local :: LAST.Name -> LAST.Type -> LAST.Operand
local name typ = LAST.LocalReference typ name


externf :: LAST.Name -> LAST.Type -> LAST.Operand
externf name typ = LAST.ConstantOperand $ LASTC.GlobalReference typ name


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

instr :: LAST.Type -> LAST.Instruction -> Codegen LAST.Operand
instr typ ins = do
  n <- fresh
  let ref = LAST.UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref LAST.:= ins) : i } )
  return $ local ref typ


instr_i :: LAST.Instruction -> Codegen LAST.Operand
instr_i = instr int


instr_d :: LAST.Instruction -> Codegen LAST.Operand
instr_d = instr double


instr_v :: LAST.Instruction -> Codegen LAST.Operand
instr_v = instr Codegen.void


instr_b :: LAST.Instruction -> Codegen LAST.Operand
instr_b = instr bool


terminator :: LAST.Named LAST.Terminator -> Codegen (LAST.Named LAST.Terminator)
terminator trm = do
  blk <- current
  case term blk of
    Nothing -> modifyBlock (blk { term = Just trm }) >> return trm
    Just x -> return x -- error $ "terminator already set"
  --return trm


fadd :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
fadd a b = instr_d $ LAST.FAdd LAST.NoFastMathFlags a b []
fsub :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
fsub a b = instr_d $ LAST.FSub LAST.NoFastMathFlags a b []
fmul :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
fmul a b = instr_d $ LAST.FMul LAST.NoFastMathFlags a b []
fdiv :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
fdiv a b = instr_d $ LAST.FDiv LAST.NoFastMathFlags a b []

iadd :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
iadd a b = instr_i $ LAST.Add True False a b []
isub :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
isub a b = instr_i $ LAST.Sub True False a b []
imul :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
imul a b = instr_i $ LAST.Mul True False a b []
idiv :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
idiv a b = instr_i $ LAST.SDiv False a b []


fcmp :: LASTFP.FloatingPointPredicate -> LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
fcmp cond a b = instr_d $ LAST.FCmp cond a b []


icmp :: LASTIP.IntegerPredicate -> LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
icmp cond a b = instr_i $ LAST.ICmp cond a b []


bAnd :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
bAnd a b = instr_b $ LAST.And a b []


bOr :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
bOr a b = instr_b $ LAST.Or a b []

br :: LAST.Name -> Codegen (LAST.Named LAST.Terminator)
br val = terminator $ LAST.Do $ LAST.Br val []


cbr :: LAST.Operand -> LAST.Name -> LAST.Name -> Codegen (LAST.Named LAST.Terminator)
cbr cond tr fl = terminator $ LAST.Do $ LAST.CondBr cond tr fl []


ret :: LAST.Operand -> Codegen (LAST.Named LAST.Terminator)
ret val = terminator $ LAST.Do $ LAST.Ret (Just val) []


retVoid :: Codegen (LAST.Named LAST.Terminator)
retVoid = terminator $ LAST.Do $ LAST.Ret Nothing []


toArgs :: [LAST.Operand] -> [(LAST.Operand, [LASTA.ParameterAttribute])]
toArgs = map (\x -> (x, []))


call :: LAST.Operand -> [LAST.Operand] -> LAST.Type -> Codegen LAST.Operand
call fn args typ = instr typ $ LAST.Call Nothing LASTCC.C [] (Right fn) (toArgs args) [] []


alloca :: LAST.Type -> Codegen LAST.Operand
alloca ty = instr ty $ LAST.Alloca ty Nothing 0 []


store :: LAST.Operand -> LAST.Operand -> Codegen LAST.Operand
store ptr@(LAST.LocalReference typ _) val = instr typ $ LAST.Store False ptr val Nothing 0 []
store _ _ = error "CODEGEN ERROR: store paramater must be a localreference"


load :: LAST.Operand -> Codegen LAST.Operand
load ptr@(LAST.LocalReference typ _) = instr typ $ LAST.Load False ptr Nothing 0 []
load _ = error "CODEGEN ERROR: load paramater must be a localreference"


sitofp :: LAST.Operand -> Codegen LAST.Operand
sitofp ptr = instr_d $ LAST.SIToFP ptr double []


cons :: LASTC.Constant -> LAST.Operand
cons = LAST.ConstantOperand


phi :: LAST.Type -> [(LAST.Operand, LAST.Name)] -> Codegen LAST.Operand
phi ty incoming = instr ty $ LAST.Phi ty incoming []


getElementPtr :: LAST.Operand -> [LAST.Operand] -> LAST.Type -> Codegen LAST.Operand
getElementPtr addr indicies typ = instr typ $ LAST.GetElementPtr True addr indicies []
