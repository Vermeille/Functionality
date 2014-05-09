module Arithmetics where

import VM
import Stack
import Opcodes
import Control.Applicative
import Control.Monad.State

evalOp :: Arith -> State Memory ()
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

