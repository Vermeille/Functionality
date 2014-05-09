module Branching where

import VM
import Stack
import Opcodes
import Control.Lens
import Control.Monad
import Control.Monad.State

valToInt :: Value -> Int
valToInt (I8 i) = fromIntegral i
valToInt (I16 i) = fromIntegral i
valToInt (I32 i) = fromIntegral i
valToInt _ = error "not an int"

evalBranch :: BranchOp -> State Memory ()
branchIf cmp dst = do
        val <- pop
        when (valToInt val `cmp` 0) $
            pc .= dst - 1

evalBranch (Beq dst) = branchIf (==) dst
evalBranch (Bneq dst) = branchIf (/=) dst
