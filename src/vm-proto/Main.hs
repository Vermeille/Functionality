{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Sequence as S
import Control.Monad.State
import Control.Lens
import Control.Applicative
import Data.Maybe (fromJust)

data Value  = I8 Int
            | I16 Int
            | I32 Int
            | F Float
            | Ptr (Int, (FunEnv -> [Value]), Int)

data FunEnv = FunEnv { _loc :: [Value], _args :: [Value], _temps :: [Value] }
data Memory = Memory { _stack :: S.Seq FunEnv }

data Arith = Add | Sub | Mul | Div
data LdOp = Ldloc Int | Ldloca Int | Ldarg Int | Lda Int

makeLenses ''FunEnv
makeLenses ''Memory

topFun = stack . _last
topTemp = temps . _head
tos = topFun . topTemp

push :: Value -> State Memory ()
push v = state $ \mem -> ((), mem & stack . _last . temps  %~ (v:))

pop :: State Memory Value
pop = state $ \mem -> (topVal mem, mem & stack . _last . temps %~ tail)
    where
        topVal mem = case mem ^? topFun . topTemp of
                        Just val -> val
                        Nothing -> error "poping an empty stack"

evalLd :: LdOp -> State Memory ()
evalLd (Ldloc n) = preuse (stack . _last . loc . ix n) >>= push . fromJust
evalLd (Ldloca n) = do
            stackLevel <- uses (stack) S.length
            push (Ptr (stackLevel, _loc, n))
evalLd (Ldarg n) = preuse (stack . _last . args . ix n) >>= push . fromJust
evalLd (Lda n) =
        do
            Ptr (s', ty, n) <- pop
            val <- preuse (stack . ix s' . to ty . ix n)
            push (fromJust val)

evalOp Add = add' <$> pop <*> pop >>= push
    where
        add' (I8 a) (I8 b) = I8 (a + b)
        add' (I8 a) (I16 b) = I16 (a + b)
        add' (I8 a) (I32 b) = I32 (a + b)
        add' (I8 a) (F b) = F (fromIntegral a + b)
        add' (I16 a) (I8 b) = I16 (a + b)
        add' (I16 a) (I16 b) = I16 (a + b)
        add' (I16 a) (I32 b) = I32 (a + b)
        add' (I16 a) (F b) = F (fromIntegral a + b)
        add' (I32 a) (I8 b) = I32 (a + b)
        add' (I32 a) (I16 b) = I32 (a + b)
        add' (I32 a) (I32 b) = I32 (a + b)
        add' (I32 a) (F b) = F (fromIntegral a + b)
        add' (F a) (I8 b) = F (a + fromIntegral b)
        add' (F a) (I16 b) = F (a + fromIntegral b)
        add' (F a) (I32 b) = F (a + fromIntegral b)
        add' (F a) (F b) = F (a + b)
        add' _ _ = error "invalid types for Add"

evalOp Sub = sub' <$> pop <*> pop >>= push
    where
        sub' (I8 a) (I8 b) = I8 (a - b)
        sub' (I8 a) (I16 b) = I16 (a - b)
        sub' (I8 a) (I32 b) = I32 (a - b)
        sub' (I8 a) (F b) = F (fromIntegral a - b)
        sub' (I16 a) (I8 b) = I16 (a - b)
        sub' (I16 a) (I16 b) = I16 (a - b)
        sub' (I16 a) (I32 b) = I32 (a - b)
        sub' (I16 a) (F b) = F (fromIntegral a - b)
        sub' (I32 a) (I8 b) = I32 (a - b)
        sub' (I32 a) (I16 b) = I32 (a - b)
        sub' (I32 a) (I32 b) = I32 (a - b)
        sub' (I32 a) (F b) = F (fromIntegral a - b)
        sub' (F a) (I8 b) = F (a - fromIntegral b)
        sub' (F a) (I16 b) = F (a - fromIntegral b)
        sub' (F a) (I32 b) = F (a - fromIntegral b)
        sub' (F a) (F b) = F (a - b)
        sub' _ _ = error "invalid types for Sub"

evalOp Mul = mul' <$> pop <*> pop >>= push
    where
        mul' (I8 a) (I8 b) = I8 (a * b)
        mul' (I8 a) (I16 b) = I16 (a * b)
        mul' (I8 a) (I32 b) = I32 (a * b)
        mul' (I8 a) (F b) = F (fromIntegral a * b)
        mul' (I16 a) (I8 b) = I16 (a * b)
        mul' (I16 a) (I16 b) = I16 (a * b)
        mul' (I16 a) (I32 b) = I32 (a * b)
        mul' (I16 a) (F b) = F (fromIntegral a * b)
        mul' (I32 a) (I8 b) = I32 (a * b)
        mul' (I32 a) (I16 b) = I32 (a * b)
        mul' (I32 a) (I32 b) = I32 (a * b)
        mul' (I32 a) (F b) = F (fromIntegral a * b)
        mul' (F a) (I8 b) = F (a * fromIntegral b)
        mul' (F a) (I16 b) = F (a * fromIntegral b)
        mul' (F a) (I32 b) = F (a * fromIntegral b)
        mul' (F a) (F b) = F (a * b)
        mul' _ _ = error "invalid types for Mul"

evalOp Div = div' <$> pop <*> pop >>= push
    where
        div' (I8 a) (I8 b) = I8 (a `div` b)
        div' (I8 a) (I16 b) = I16 (a `div` b)
        div' (I8 a) (I32 b) = I32 (a `div` b)
        div' (I8 a) (F b) = F (fromIntegral a / b)
        div' (I16 a) (I8 b) = I16 (a `div` b)
        div' (I16 a) (I16 b) = I16 (a `div` b)
        div' (I16 a) (I32 b) = I32 (a `div` b)
        div' (I16 a) (F b) = F (fromIntegral a / b)
        div' (I32 a) (I8 b) = I32 (a `div` b)
        div' (I32 a) (I16 b) = I32 (a `div` b)
        div' (I32 a) (I32 b) = I32 (a `div` b)
        div' (I32 a) (F b) = F (fromIntegral a / b)
        div' (F a) (I8 b) = F (a / fromIntegral b)
        div' (F a) (I16 b) = F (a / fromIntegral b)
        div' (F a) (I32 b) = F (a / fromIntegral b)
        div' (F a) (F b) = F (a / b)
        div' _ _ = error "invalid types for Div"

main = return ()
