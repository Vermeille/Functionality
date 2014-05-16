module Stack where

import VM
import Opcodes
import Control.Lens
import Control.Monad.State
import qualified Data.Sequence as S
import Data.Maybe (fromJust, fromMaybe)

push :: Value -> State Memory ()
push v = state $ \mem -> ((), mem & topFun . temps  %~ (v:))

pop :: State Memory Value
pop = state $ \mem -> (topVal mem, mem & topFun . temps %~ tail)
    where
        topVal mem = fromMaybe (error "poping an empty stack") $ mem ^? topFun . topTemp

evalPush :: PushOp -> State Memory ()
evalPush (Pushimm v) = push v

evalLd :: LdOp -> State Memory ()
evalLd (Ldloc n) = preuse (topFun . loc . ix n) >>= push . fromJust
evalLd (Ldloca n) = do
            stackLevel <- uses stack S.length
            push (Ptr (stackLevel, Local, n))
evalLd (Ldarg n) = preuse (topFun . args . ix n) >>= push . fromJust
evalLd (Lda n) = do
            Ptr (s', ty, n) <- pop
            val <- preuse (stack . ix s' . to (takeVar ty) . ix n)
            push $ fromJust val
evalLd (Construct uid cid) = do
            udef <- getUnionDef
            let Just uMembs = (udef ^? ctors . ix cid . ctorMembers)
            let nbMembs = length uMembs
            args' <- uses (topFun . temps) (take nbMembs)
            topFun . temps %= drop nbMembs
            _ <- return $ zipWith typeCheck args' uMembs
            evalPush $ Pushimm (Union uid args')
            where
                getUnionDef :: State Memory TyUnion
                getUnionDef = preuse (types . ix uid) >>=
                                return . fromMaybe (error ("union #" ++ show uid ++ "doesnt exist"))

evalLd Dup = preuse tos >>= push . fromMaybe (error "nothing on tos")

