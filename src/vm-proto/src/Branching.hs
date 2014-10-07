module Branching where

import VM
import Opcodes
import Control.Monad
import System.IO.Unsafe
import Control.Lens hiding ((|>))
import Control.Monad.State
import qualified Data.Sequence as S
import Data.Maybe (fromJust)

-- | extract an int out of an i{8,16,32} value. Useful when casting or wanting
-- to disregard int type
valToInt :: Value -> Int
valToInt (I8 i) = fromIntegral i
valToInt (I16 i) = fromIntegral i
valToInt (I32 i) = fromIntegral i
valToInt _ = error "not an int"

-- | Utility function to make evalBranch cleaner
branchIf :: (Int -> Int -> Bool) -- ^ A comparison function
            -> Int               -- ^ Destination address
            -> State VM ()   -- ^ The VM
branchIf cmp dst = do
        val <- pop
        when (valToInt val `cmp` 0) $
            pc._instr .= dst - 1

-- | Pop a value
popFun :: State VM ()
popFun = do
        st <- use stack
        case S.viewr st of
            S.EmptyR -> error "trying to pop an empty stack. That should NEVER happen"
            previous S.:> cur -> do
                stackfree ((length . _temps $ cur) + (length . _args $ cur))
                stack .= previous

-- | Evaluate a branching instruction
evalBranch :: BranchOp -> State VM ()
evalBranch (Beq dst) = branchIf (==) dst
evalBranch (Bneq dst) = branchIf (/=) dst
evalBranch (Blt dst) = branchIf (<) dst
evalBranch (Bltq dst) = branchIf (<=) dst
evalBranch (Bgt dst) = branchIf (>) dst
evalBranch (Bgtq dst) = branchIf (>=) dst
evalBranch (Jmp dst) = pc._instr .= dst - 1
evalBranch Ret = do
        ret <- tos
        Just (funPc, instrPc) <- preuse (topFun . retAddr)
        popFun
        _ <- push ret
        pc .= (funPc, instrPc - 1)
evalBranch (Call funId) = do
        Just tyArgs <- preuse (nthFun funId . params)
        args'' <- preuses (topFun . temps) (take (length tyArgs)) >>=
            mapM readMem . fromJust
        topFun . temps %= drop (length tyArgs)
        _ <- return $ zipWith typeCheck args'' tyArgs
        Just funDef <- preuse (nthFun funId)
        Just (curFun, curInstr) <- preuse pc
        newFrame <- newStackFrame funDef args'' (curFun, curInstr + 1)
        stack %= (S.|> newFrame)
        pc .= (funId, -1)
evalBranch (BreakPoint) = do
        vm <- get
        return $! (unsafePerformIO $ putStrLn (show vm))
evalBranch (Match cid dst) = do
        val <- tos
        case val of
            Union cid' _ ->
                if cid == cid' then
                    evalBranch (Jmp dst)
                else
                    return $! ()
            _ -> error "not an Union on top of stack"

