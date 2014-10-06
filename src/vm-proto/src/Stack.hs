module Stack where

import VM
import Opcodes
import Control.Lens
import Control.Monad.State
import qualified Data.IntMap as I
import qualified Data.Sequence as S
import Data.Maybe (fromJust, fromMaybe)

-- | evaluate push instruction
evalPush :: PushOp -> State VM ()
evalPush (Pushimm v) = push v >>= \_ -> return ()

-- | evaluate load instructions
evalLd :: LdOp -> State VM ()
evalLd (Ldloc n) = do
        Just addr <- preuse (topFun . loc . ix n)
        val <- readMem addr
        push val
        return ()
evalLd (Ldloca n) = do
            stackLevel <- uses stack S.length
            undefined -- FIXME: push (Ptr (Local, [stackLevel, n]))
evalLd (Ldarg n) = do
        Just addr <- preuse (topFun . args . ix n)
        val <- readMem addr
        push val
        return ()
{- FIXME
evalLd (Lda n) = do
            Ptr (ty, s':n:n') <- pop
            val <- preuse (stack . ix s' . to (takeVar ty) . ix n . ixUnion n')
            push $ fromJust val
-}
evalLd (Construct uid cid) = do
            udef <- getUnionDef
            let Just uMembs = udef ^? ctors . ix cid . ctorMembers
            let nbMembs = length uMembs
            args' <- uses (topFun . temps) (take nbMembs)
            args'' <- mapM readMem args'
            topFun . temps %= drop nbMembs
            _ <- return $ zipWith typeCheck args'' uMembs
            evalPush $ Pushimm (Union cid args'')
            where
                getUnionDef :: State VM TyUnion
                getUnionDef = preuse (types . ix uid) >>=
                                return . fromMaybe (error ("union #" ++ show uid ++ "doesnt exist"))
evalLd (Ldslot n) = do
        val <- tos
        case val of
            Union _ vals -> evalPush $ Pushimm (vals !! n)
            _ -> error "not an Union on top of stack"
evalLd (Ldslota n) = do
            val <- tos
            case val of
                Union _ _ -> do
                    stackLevel <- uses stack S.length
                    Just tempLevel <- preuses (topFun . temps) length
                    undefined -- FIXME: push $ Ptr (Temp, [stackLevel, tempLevel, n])
                {- FIXME
                Ptr (ty, ptrs) -> do
                    _ <- pop
                    undefined -- FIXME: push $ Ptr (ty, ptrs ++ [n])
                -}
                _ -> error $ show val ++ " is not a proper value for ldslota"
evalLd (Ldarga n) = do
        stackLevel <- uses stack S.length
        undefined -- FIXME: push $ Ptr (Arg, [stackLevel, n])

evalLd Dup = tos >>= push >>= \_ -> return ()

evalSt :: StOp -> State VM ()
evalSt (Stloc addr) = do
    val <- pop
    Just locAddr <- preuse (topFun . loc . ix addr)
    storeMem locAddr val
evalSt (Stlocat n) = do
    Just val <- preuse (topFun . loc . ix n)
    addr <- pop
    undefined -- FIXME: ixPtr addr .= val
evalSt (Starg addr) = do
    val <- pop
    Just argAddr <- preuse $ topFun . args . ix addr
    storeMem argAddr val
evalSt (Stargate n) = do
    Just val <- preuse (topFun . args . ix n)
    addr <- pop
    undefined -- FIXME: ixPtr addr .= val

