{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module VM where

import Opcodes
import Control.Lens
import qualified Data.Vector as V
import qualified Data.Sequence as S

data FunEnv = FunEnv { _loc :: [Value]
                     , _args :: [Value]
                     , _temps :: [Value]
                     , _retAddr :: (Int, Int) }
            deriving (Show)

data Memory = Memory { _stack :: S.Seq FunEnv
                     , _pc :: (Int, Int)
                     , _funs :: V.Vector Function}
            deriving (Show)

data Function = Function { _name :: String
                         , _params :: [Value]
                         , _impl :: V.Vector Opcode }
            deriving (Show)

makeLenses ''FunEnv
makeLenses ''Memory
makeLenses ''Function

newStackFrame :: [Value] -> FunEnv
newStackFrame param = FunEnv { _loc = []
                             , _args = param
                             , _temps = []
                             , _retAddr = (-1, -1) }

newVM :: [Function] -> Memory
newVM funs' = Memory { _stack = S.fromList (replicate 2 $ newStackFrame [])
                     , _pc = (0, 0)
                     , _funs = V.fromList funs' }

newFun :: String -> [Value] -> [Opcode] -> Function
newFun name' args' impl' = Function { _name = name'
                                   , _params = args'
                                   , _impl = V.fromList impl' }

topFun :: Traversal' Memory FunEnv
topFun = stack . _last

topTemp :: Traversal' FunEnv Value
topTemp = temps . _head

tos :: Traversal' Memory Value
tos = topFun . topTemp

code :: (Int, Int) -> Traversal' Memory Opcode
code (fun, instr) = funs . ix fun . impl . ix instr

_fun :: Lens' (Int, Int) Int
_fun = _1
_instr :: Lens' (Int, Int) Int
_instr x = _2 x

takeVar :: VarType -> (FunEnv -> [Value])
takeVar Local = _loc
takeVar Arg = _args
takeVar Temp = _temps

