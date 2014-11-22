module Branching where

import VM
import Opcodes
import System.IO.Unsafe
import Control.Lens hiding ((|>))
import Control.Monad.State
import qualified Data.Sequence as S
import qualified Data.IntMap as I
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
branchIf cmp dst = modify $ branchIf' cmp dst

branchIf' :: (Int -> Int -> Bool) -> Int -> VM -> VM
branchIf' cmp dst vm =
        if valToInt popVal `cmp` 0 then
            poppedVM { _pc = newPC }
        else
            poppedVM
    where
        (popVal, poppedVM) = pop vm
        newPC = let (f, _) = _pc vm in (f, dst - 1)

-- | Pop a stack frame
popFun :: State VM ()
popFun = modify popFun'

popFun' :: VM -> VM
popFun' vm = vm { _stack = prevStackFrames
                , _mmu = freedMMU
                , _esp = newEsp }
    where
        freedMMU = foldl freeAddr (_mmu vm) allAddr
        freeAddr mmu addr = I.delete (unpackAddr addr) mmu
        allAddr = _temps curSF ++ _args curSF ++ _loc curSF
        newEsp = _esp vm - length (_temps curSF)
        (prevStackFrames, curSF) =
            case S.viewr $ _stack vm of
                S.EmptyR -> error "trying to pop an empty stack. That should NEVER happen"
                previous S.:> curr -> (previous, curr)

-- | Evaluate a branching instruction
evalBranch :: BranchOp -> State VM ()
evalBranch (Beq dst) = branchIf (==) dst
evalBranch (Bneq dst) = branchIf (/=) dst
evalBranch (Blt dst) = branchIf (<) dst
evalBranch (Bltq dst) = branchIf (<=) dst
evalBranch (Bgt dst) = branchIf (>) dst
evalBranch (Bgtq dst) = branchIf (>=) dst
evalBranch (Jmp dst) = pc._instr .= dst - 1
evalBranch Ret = modify evalRet'
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

evalRet' :: VM -> VM
evalRet' vm = newVM { _pc = newPC }
    where
        newPC = let Just (f, i) = vm ^? topFun . retAddr in (f, i - 1)
        retVal = rdMem (tos' vm) vm
        newVM = snd . push' retVal . popFun' $ vm

