{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module VM where

import Opcodes
import Control.Lens
import Data.List (intercalate)
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Sequence as S

data FunEnv = FunEnv { _loc :: [Value]
                     , _args :: [Value]
                     , _temps :: [Value]
                     , _retAddr :: (Int, Int) }
            deriving (Show)

data Memory = Memory { _stack :: S.Seq FunEnv
                     , _pc :: (Int, Int)
                     , _funs :: V.Vector Function}

data Function = Function { _name :: String
                         , _params :: [VarType]
                         , _retVal :: VarType
                         , _locVar :: [VarType]
                         , _impl :: V.Vector Opcode }

makeLenses ''FunEnv
makeLenses ''Memory
makeLenses ''Function

newStackFrame :: Function -> [Value] -> (Int, Int) -> FunEnv
newStackFrame fun' args' retPtr = FunEnv { _loc = map type2defval $ _locVar fun'
                                  , _args = args'
                                  , _temps = []
                                  , _retAddr = retPtr }

emptyStackFrame :: [Value] -> FunEnv
emptyStackFrame param = FunEnv { _loc = []
                             , _args = param
                             , _temps = []
                             , _retAddr = (-1, -1) }

newVM :: [Function] -> Memory
newVM funs' = Memory { _stack = S.fromList (replicate 2 $ emptyStackFrame [])
                     , _pc = (0, 0)
                     , _funs = V.fromList funs' }

newFun :: String -> [VarType] -> VarType -> [VarType] -> [Opcode] -> Function
newFun name' args' retVal' locs' impl' = Function { _name = name'
                                                 , _params = args'
                                                 , _retVal = retVal'
                                                 , _locVar = locs'
                                                 , _impl = V.fromList impl' }

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

takeVar :: VarPlace -> (FunEnv -> [Value])
takeVar Local = _loc
takeVar Arg = _args
takeVar Temp = _temps

type2defval :: VarType -> Value
type2defval TyI8 = I8 0
type2defval TyI16 = I16 0
type2defval TyI32 = I32 0
type2defval TyF = F 0
type2defval TyPtr = Ptr (-1, Heap, 0)

instance Show Memory where
        show m = "========== Functionality ========\n" ++
            "Stack:\n  " ++
            (intercalate "\n  " . map show . F.toList $ _stack m) ++
            "\n\nCurrently in fun " ++ (show . fst $ _pc m) ++ " at instr " ++
            (show . snd $ _pc m) ++ "\n\nCode:\n" ++
            (intercalate "\n" . map show . V.toList $ _funs m) ++ "\n"

instance Show Function where
        show f =
            _name f ++ " :: " ++
            (intercalate " -> " . map show $ _params f) ++ " -> " ++ show (_retVal f) ++
            " {\nlocals: " ++ show (_locVar f) ++ "\ncode:\n  " ++
            (intercalate "\n  " . map show . V.toList $ _impl f) ++ "\n}\n"
