module Eval where

import VM
import Stack
import Opcodes
import Branching
import Arithmetics
import Control.Lens
import Control.Monad.State
import qualified Data.Vector as V
import Data.Maybe (fromJust, fromMaybe)

eval (Ar a) = evalOp a
eval (Ld ld) = evalLd ld
eval (Push p) = evalPush p
eval (Branch b) = evalBranch b

runVM :: State Memory ()
runVM = do
        idx <- use pc
        len <- uses code V.length
        unless (idx == len) $ do
        opcode <- preuse (code . ix idx)
        eval $ fromMaybe (error "invalid pc") opcode
        pc += 1
        runVM

