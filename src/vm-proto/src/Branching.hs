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

branchIf :: (Int -> Int -> Bool) -> Int -> State Memory ()
branchIf cmp dst = do
        val <- pop
        when (valToInt val `cmp` 0) $
            pc .= dst - 1

evalBranch :: BranchOp -> State Memory ()
evalBranch (Beq dst) = branchIf (==) dst
evalBranch (Bneq dst) = branchIf (/=) dst
evalBranch (Blt dst) = branchIf (<) dst
evalBranch (Bltq dst) = branchIf (<=) dst
evalBranch (Bgt dst) = branchIf (>) dst
evalBranch (Bgtq dst) = branchIf (>=) dst
evalBranch (Jmp dst) = do pc .= dst - 1

