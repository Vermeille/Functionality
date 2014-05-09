{-# LANGUAGE TemplateHaskell #-}

module VM where

import Opcodes
import Control.Lens
import Control.Monad.State
import qualified Data.Vector as V
import qualified Data.Sequence as S

data FunEnv = FunEnv { _loc :: [Value], _args :: [Value], _temps :: [Value] }
            deriving (Show)
data Memory = Memory { _stack :: S.Seq FunEnv, _code :: V.Vector Opcode, _pc :: Int }
            deriving (Show)

makeLenses ''FunEnv
makeLenses ''Memory

newStackFrame args = FunEnv { _loc = [], _args = args, _temps = [] }
newVM code = Memory { _stack = S.fromList [newStackFrame []], _code = V.fromList code, _pc = 0 }

topFun x = (stack . _last) x
topTemp x = (temps . _head) x
tos x = (topFun . topTemp) x

takeVar Local = _loc
takeVar Arg = _args
takeVar Temp = _temps

