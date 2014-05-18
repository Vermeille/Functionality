module Stack where

import VM
import Opcodes
import Control.Lens
import Control.Monad.State
import qualified Data.Sequence as S
import Data.Maybe (fromJust, fromMaybe)

-- | Push a value on the stack
push :: Value -> State Memory ()
push v = state $ \mem -> ((), mem & topFun . temps  %~ (v:))

-- | Pop a value from the stack
pop :: State Memory Value
pop = state $ \mem -> (topVal mem, mem & topFun . temps %~ tail)
    where
        topVal mem = fromMaybe (error "poping an empty stack") $ mem ^? topFun . topTemp

-- | evaluate push instruction
evalPush :: PushOp -> State Memory ()
evalPush (Pushimm v) = push v

-- | evaluate load instructions
evalLd :: LdOp -> State Memory ()
evalLd (Ldloc n) = preuse (topFun . loc . ix n) >>= push . fromJust
evalLd (Ldloca n) = do
            stackLevel <- uses stack S.length
            push (Ptr (Local, [stackLevel, n]))
evalLd (Ldarg n) = preuse (topFun . args . ix n) >>= push . fromJust
evalLd (Lda n) = do
            Ptr (ty, s':n:n') <- pop
            val <- preuse (stack . ix s' . to (takeVar ty) . ix n . ixUnion n')
            push $ fromJust val
evalLd (Construct uid cid) = do
            udef <- getUnionDef
            let Just uMembs = (udef ^? ctors . ix cid . ctorMembers)
            let nbMembs = length uMembs
            args' <- uses (topFun . temps) (take nbMembs)
            topFun . temps %= drop nbMembs
            _ <- return $ zipWith typeCheck args' uMembs
            evalPush $ Pushimm (Union cid args')
            where
                getUnionDef :: State Memory TyUnion
                getUnionDef = preuse (types . ix uid) >>=
                                return . fromMaybe (error ("union #" ++ show uid ++ "doesnt exist"))
evalLd (Ldslot n) = do
        Just val <- preuse tos
        case val of
            Union _ vals -> evalPush $ Pushimm (vals !! n)
            _ -> error "not an Union on top of stack"
evalLd (Ldslota n) = do
            Just val <- preuse tos
            case val of
                Union _ _ -> do
                    stackLevel <- uses stack S.length
                    Just tempLevel <- preuses (topFun . temps) length
                    push $ Ptr (Temp, [stackLevel, tempLevel, n])
                Ptr (ty, ptrs) -> do
                    _ <- pop
                    push $ Ptr (ty, ptrs ++ [n])
                _ -> error $ show val ++ " is not a proper value for ldslota"
evalLd (Ldarga n) = do
        stackLevel <- uses stack S.length
        push $ Ptr (Arg, [stackLevel, n])

evalLd Dup = preuse tos >>= push . fromMaybe (error "nothing on tos")

evalSt :: StOp -> State Memory ()
evalSt (Stloc addr) = do
    val <- pop
    topFun . loc . ix addr .= val
evalSt (Stlocat n) = do
    Just val <- preuse (topFun . loc . ix n)
    addr <- pop
    ixPtr addr .= val
evalSt (Starg addr) = do
    val <- pop
    topFun . args . ix addr .= val
evalSt (Stargate n) = do
    Just val <- preuse (topFun . args . ix n)
    addr <- pop
    ixPtr addr .= val

