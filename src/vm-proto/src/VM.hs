{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module VM where

import Opcodes
import Control.Lens
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Vector as V
import qualified Data.IntMap as I
import qualified Data.Foldable as F
import qualified Data.Sequence as S

-- | You don't want to confuse a number with an address
newtype Addr = Addr { unpackAddr :: Int} deriving (Show)

toAddr :: Int -> Addr
toAddr = Addr

-- | Isomorphic to a stack frame, defines a function execution context
data FunEnv = FunEnv { _loc     :: [Addr]  -- ^ local variables
                     , _args    :: [Addr]  -- ^ arguments
                     , _temps   :: [Addr]  -- ^ stack of temp values
                     , _retAddr :: (Int, Int) -- ^ return address of the caller
                     }
            deriving (Show)

-- | Root datastructure of the VM
data VM = VM { _stack :: S.Seq FunEnv       -- ^ stack of stack frames
             , _pc    :: (Int, Int)         -- ^ program counter as
                                            --   (functionId, instrNbr)
             , _funs  :: V.Vector Function  -- ^ Functions of the prgm
             , _types :: V.Vector TyUnion   -- ^ Types defined by prgm
             , _mmu   :: I.IntMap Value     -- ^ gives the actual Value linked
                                            --   to the ValueIxs
             , _esp   :: Int                -- ^ index which points to the top
                                            --   of the stack in MMU
             }

-- | What a function of the program is
data Function = Function { _name   :: String          -- ^ It has a name
                         , _params :: [VarType]       -- ^ Some param types
                         , _retVal :: VarType         -- ^ A return type
                         , _locVar :: [VarType]       -- ^ some local variables
                         , _impl   :: V.Vector Opcode -- ^ an implementation
                         }

makeLenses ''FunEnv
makeLenses ''VM
makeLenses ''Function

-- | Utility function to create a VM from a set of functions and a set of types
newVM :: [Function] -> [TyUnion] -> VM
newVM funs' types' = VM { _stack = S.fromList (replicate 2 $ emptyStackFrame [])
                     , _pc = (0, 0)
                     , _funs = V.fromList funs'
                     , _types = V.fromList types'
                     , _mmu = I.empty
                     , _esp = 0
                     }

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

stackalloc :: State VM Addr
stackalloc = do
        prevEsp <- use esp
        esp %= (+ 1)
        return . toAddr $ prevEsp

stackfree :: Int -> State VM ()
stackfree n = esp %= (subtract n)

-- | Push a value on the stack
push :: Value -> State VM Addr
push (Union _ _) = undefined
push v = do
        addr <- stackalloc
        topFun . temps  %= (addr :)
        storeMem addr v
        return addr

-- | Pop a value from the stack
pop :: State VM Value
pop = do
        tosIdx <- gets tos'
        val <- readMem tosIdx
        mmu %= I.delete (unpackAddr tosIdx)
        topFun . temps %= tail
        _ <- stackfree 1
        return val

-- | Utility function to create a stack frame / FunEnv from arguments and
--   return address
newStackFrame :: Function               -- ^ the fun to call
                 -> [Value]             -- ^ its parameters
                 -> (Int, Int)          -- ^ the curent pc
                 -> State VM FunEnv     -- ^ the stack frame
newStackFrame fun' args' retPtr = do
        pars <- mapM push args'
        let loc' = map type2defval $ _locVar fun'
        ploc <- mapM push loc'
        return FunEnv
                  { _loc = ploc
                  , _args = pars
                  , _temps = []
                  , _retAddr = retPtr }

-- | Utility function to create an empty stack frame which will terminate the
--   VM when returned to (Used to create Main's stack frame)
emptyStackFrame :: [Addr]       -- ^ Arguments (ie command line flags)
                   -> FunEnv    -- ^ the stack frame
emptyStackFrame param = FunEnv { _loc = []
                             , _args = param
                             , _temps = []
                             , _retAddr = (-1, -1) }

rdMem :: Addr -> VM -> Value
rdMem addr vm = _mmu vm I.! unpackAddr addr

rdLocation :: (VM -> Addr) -> VM -> Value
rdLocation addr vm = rdMem (addr vm) vm

stMem :: Addr -> Value -> VM -> VM
stMem addr v vm = vm { _mmu = I.insert (unpackAddr addr) v (_mmu vm) }

stLocation :: (VM -> Addr) -> Value -> VM -> VM
stLocation addr v vm = stMem (addr vm) v vm

readMem :: Addr -> State VM Value
readMem addr = gets (rdMem addr)

storeMem :: Addr -> Value -> State VM ()
storeMem addr val = modify (stMem addr val)

-- Some lenses to make the code cleaner
topFun :: Traversal' VM FunEnv
topFun = stack . _last

topTemp :: Traversal' FunEnv Addr
topTemp = temps . _head

tos' :: VM -> Addr
tos' vm = let Just addr = vm ^? (topFun . topTemp) in addr

tos :: State VM Value
tos = gets tos' >>= readMem

nthFun :: Int -> Traversal' VM Function
nthFun n = funs . ix n

code :: (Int, Int) -> Traversal' VM Opcode
code (fun, instr) = nthFun fun . impl . ix instr

_fun :: Lens' (Int, Int) Int
_fun = _1
_instr :: Lens' (Int, Int) Int
_instr = _2

type2defval :: VarType -> Value
type2defval TyI8 = I8 0
type2defval TyI16 = I16 0
type2defval TyI32 = I32 0
type2defval TyF = F 0
type2defval TyPtr = Ptr 0
-- type2defval (UnionId uid) = Union uid [] -- membs

-- FIXME: Please, make me readable! looks like perl!
instance Show VM where
        show m = "========== Functionality ========\n" ++
            "Stack\n-----\nesp: " ++ (show $ _esp m) ++ "\n" ++
            (intercalate"\n" . map show . F.toList $ _stack m) ++ "\n" ++
            (intercalate"\n  " . map show . I.toList $ _mmu m) ++ "\n" ++
            "\n\nCurrently in fun " ++ (show . fst $ _pc m) ++ " at instr " ++
            (show . snd $ _pc m) ++ "\n\nCode\n----\n\n" ++
            (intercalate "\n" . map show . V.toList $ _funs m) ++ "\n\nTypes\n"
            ++ "-----\n\n" ++ (intercalate "\n" . map show . V.toList $ _types m)
                -- where
                    -- printFuns vm = pfs vm 0
                    -- pfs vm n =
                    -- pf _funs m V.! n

instance Show Function where
        show f =
            _name f ++ " :: " ++
            (intercalate " -> " . map show $ _params f) ++ " -> " ++ show (_retVal f) ++
            " {\nlocals: " ++ show (_locVar f) ++ "\ncode:\n  " ++
            (intercalate "\n  " . map show . V.toList $ _impl f) ++ "\n}\n"

