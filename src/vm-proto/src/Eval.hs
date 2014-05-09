module Eval where

import VM
import Stack
import Opcodes
import Branching
import Arithmetics
import Control.Lens
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Debug.Trace

eval :: Opcode -> State Memory ()
eval (Ar a) = evalOp a
eval (Ld ld) = evalLd ld
eval (Push p) = evalPush p
eval (Branch b) = evalBranch b

runVM :: State Memory ()
runVM = do
        idx <- use pc
        unless (fst idx == -1) $ do
        opcode <- preuse (code idx)
        eval $ fromMaybe (error "invalid pc") opcode
        pc . _instr += 1
        runVM

