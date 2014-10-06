module Eval where

import VM
import Stack
import Opcodes
import Branching
import Arithmetics
import Control.Lens
import Control.Monad.State
import Data.Maybe (fromMaybe)

-- | Evaluate an Opcode
eval :: Opcode -> State VM ()
eval (Ar a) = evalOp a >>= \_ -> return ()
eval (Ld ld) = evalLd ld
eval (Push p) = evalPush p
eval (Branch b) = evalBranch b
eval (St s) = evalSt s

-- | The main function: executes the code until pc is set to -1
runVM :: State VM ()
runVM = do
        idx <- use pc
        unless (fst idx == -1) $ do
            opcode <- preuse (code idx)
            eval $ fromMaybe (error "invalid pc") opcode
            pc . _instr += 1
            runVM

stepVM :: State VM ()
stepVM = do
        idx <- use pc
        unless (fst idx == -1) $ do
            opcode <- preuse (code idx)
            eval $ fromMaybe (error "invalid pc") opcode
            pc . _instr += 1

