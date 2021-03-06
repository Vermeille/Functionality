{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module VM where

import Opcodes
import Control.Lens
import Data.List (intercalate)
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Sequence as S

-- | Isomorphic to a stack frame, defines a function execution context
data FunEnv = FunEnv { _loc :: [Value]        -- ^ local variables
                     , _args :: [Value]       -- ^ arguments
                     , _temps :: [Value]      -- ^ stack of temp values
                     , _retAddr :: (Int, Int) -- ^ return address of the caller
                     }
            deriving (Show)

-- | Will certainly be renamed as VM, root datastructure of the VM
data Memory = Memory { _stack :: S.Seq FunEnv       -- ^ stack of stack frames
                     , _pc :: (Int, Int)            -- ^ program counter as
                                                    --   (functionId, instrNbr)
                     , _funs :: V.Vector Function   -- ^ Functions of the prgm
                     , _types :: V.Vector TyUnion}  -- ^ Types defined by prgm

-- | What a function of the program is
data Function = Function { _name :: String          -- ^ It has a name
                         , _params :: [VarType]     -- ^ Some param types
                         , _retVal :: VarType       -- ^ A return type
                         , _locVar :: [VarType]     -- ^ some local variables
                         , _impl :: V.Vector Opcode -- ^ an implementation
                         }

makeLenses ''FunEnv
makeLenses ''Memory
makeLenses ''Function

-- | Utility function to create a stack frame / FunEnv from arguments and
--   return address
newStackFrame :: Function       -- ^ the fun to call
                 -> [Value]     -- ^ its parameters
                 -> (Int, Int)  -- ^ the curent pc
                 -> FunEnv      -- ^ the stack frame
newStackFrame fun' args' retPtr = FunEnv
                                  { _loc = map type2defval $ _locVar fun'
                                  , _args = args'
                                  , _temps = []
                                  , _retAddr = retPtr }

-- | Utility function to create an empty stack frame which will terminate the
--   VM when returned to (Used to create Main's stack frame)
emptyStackFrame :: [Value]      -- ^ Arguments (ie command line flags)
                   -> FunEnv    -- ^ the stack frame
emptyStackFrame param = FunEnv { _loc = []
                             , _args = param
                             , _temps = []
                             , _retAddr = (-1, -1) }

-- | Utility function to create a VM from a set of functions and a set of types
newVM :: [Function] -> [TyUnion] -> Memory
newVM funs' types' = Memory { _stack = S.fromList (replicate 2 $ emptyStackFrame [])
                     , _pc = (0, 0)
                     , _funs = V.fromList funs'
                     , _types = V.fromList types'}

-- | Utility function to create a function
newFun :: String        -- ^ Its name
          -> [VarType]  -- ^ Its arguments' types
          -> VarType    -- ^ The return value's type
          -> [VarType]  -- ^ types of local variables
          -> [Opcode]   -- ^ its implementation
          -> Function   -- ^ the resulting object
newFun name' args' retVal' locs' impl' = Function { _name = name'
                                                 , _params = args'
                                                 , _retVal = retVal'
                                                 , _locVar = locs'
                                                 , _impl = V.fromList impl' }
-- Some lenses to make the code cleaner
topFun :: Traversal' Memory FunEnv
topFun = stack . _last

topTemp :: Traversal' FunEnv Value
topTemp = temps . _head

tos :: Traversal' Memory Value
tos = topFun . topTemp

nthFun :: Int -> Traversal' Memory Function
nthFun n = funs . ix n

code :: (Int, Int) -> Traversal' Memory Opcode
code (fun, instr) = nthFun fun . impl . ix instr

_fun :: Lens' (Int, Int) Int
_fun = _1
_instr :: Lens' (Int, Int) Int
_instr = _2

ixUnion :: [Int] -> Traversal' Value Value
ixUnion [] = id
ixUnion (ptr:ptrs) = unionValues . ix ptr . ixUnion ptrs

ixPtr :: Value -> Traversal' Memory Value
ixPtr (Ptr (Local, frameIx:locIx:unionIx)) =
    stack . ix frameIx . loc . ix locIx . ixUnion unionIx
ixPtr (Ptr (Arg, frameIx:argIx:unionIx)) =
    stack . ix frameIx . args . ix argIx . ixUnion unionIx
ixPtr (Ptr (Temp, frameIx:argIx:unionIx)) =
    stack . ix frameIx . temps . ix argIx . ixUnion unionIx

takeVar :: VarPlace -> (FunEnv -> [Value])
takeVar Local = _loc
takeVar Arg = _args
takeVar Temp = _temps

type2defval :: VarType -> Value
type2defval TyI8 = I8 0
type2defval TyI16 = I16 0
type2defval TyI32 = I32 0
type2defval TyF = F 0
type2defval TyPtr = Ptr (Heap, [])
type2defval (UnionId uid) = Union uid [] -- membs

-- FIXME: Please, make me readable! looks like perl!
instance Show Memory where
        show m = "========== Functionality ========\n" ++
            "Stack\n-----\n" ++
            (intercalate "\n  " . map show . F.toList $ _stack m) ++
            "\n\nCurrently in fun " ++ (show . fst $ _pc m) ++ " at instr " ++
            (show . snd $ _pc m) ++ "\n\nCode\n----\n\n" ++
            (intercalate "\n" . map show . V.toList $ _funs m) ++ "\n\nTypes\n"
            ++ "-----\n\n" ++ (intercalate "\n" . map show . V.toList $ _types m)

instance Show Function where
        show f =
            _name f ++ " :: " ++
            (intercalate " -> " . map show $ _params f) ++ " -> " ++ show (_retVal f) ++
            " {\nlocals: " ++ show (_locVar f) ++ "\ncode:\n  " ++
            (intercalate "\n  " . map show . V.toList $ _impl f) ++ "\n}\n"

