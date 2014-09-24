module Arithmetics where

import VM
import Stack
import Opcodes
import Data.Bits
import Control.Applicative
import Control.Monad.State

-- | Evaluate an arithmetical operation
evalOp :: Arith -> State VM ()
evalOp Add = add' <$> pop <*> pop >>= push
    where
        add' (I8 a) (I8 b) = I8 (a + b)
        add' (I8 a) (I16 b) = I16 (fromIntegral a + b)
        add' (I8 a) (I32 b) = I32 (fromIntegral a + b)
        add' (I8 a) (F b) = F (fromIntegral a + b)
        add' (I16 a) (I8 b) = I16 (a + fromIntegral b)
        add' (I16 a) (I16 b) = I16 (a + b)
        add' (I16 a) (I32 b) = I32 (fromIntegral a + b)
        add' (I16 a) (F b) = F (fromIntegral a + b)
        add' (I32 a) (I8 b) = I32 (a + fromIntegral b)
        add' (I32 a) (I16 b) = I32 (a + fromIntegral b)
        add' (I32 a) (I32 b) = I32 (a + fromIntegral b)
        add' (I32 a) (F b) = F (fromIntegral a + b)
        add' (F a) (I8 b) = F (a + fromIntegral b)
        add' (F a) (I16 b) = F (a + fromIntegral b)
        add' (F a) (I32 b) = F (a + fromIntegral b)
        add' (F a) (F b) = F (a + b)
        add' _ _ = error "invalid types for Add"

evalOp Sub = do
        a <- pop
        b <- pop
        push (b `sub'` a)
    where
        sub' (I8 a) (I8 b) = I8 (a - b)
        sub' (I8 a) (I16 b) = I16 (fromIntegral a - b)
        sub' (I8 a) (I32 b) = I32 (fromIntegral a - b)
        sub' (I8 a) (F b) = F (fromIntegral a - b)
        sub' (I16 a) (I8 b) = I16 (a - fromIntegral b)
        sub' (I16 a) (I16 b) = I16 (a - b)
        sub' (I16 a) (I32 b) = I32 (fromIntegral a - b)
        sub' (I16 a) (F b) = F (fromIntegral a - b)
        sub' (I32 a) (I8 b) = I32 (a - fromIntegral b)
        sub' (I32 a) (I16 b) = I32 (a - fromIntegral b)
        sub' (I32 a) (I32 b) = I32 (a - fromIntegral b)
        sub' (I32 a) (F b) = F (fromIntegral a - b)
        sub' (F a) (I8 b) = F (a - fromIntegral b)
        sub' (F a) (I16 b) = F (a - fromIntegral b)
        sub' (F a) (I32 b) = F (a - fromIntegral b)
        sub' (F a) (F b) = F (a - b)
        sub' _ _ = error "invalid types for sub"

evalOp Mul = mul' <$> pop <*> pop >>= push
    where
        mul' (I8 a) (I8 b) = I8 (a * b)
        mul' (I8 a) (I16 b) = I16 (fromIntegral a * b)
        mul' (I8 a) (I32 b) = I32 (fromIntegral a * b)
        mul' (I8 a) (F b) = F (fromIntegral a * b)
        mul' (I16 a) (I8 b) = I16 (a * fromIntegral b)
        mul' (I16 a) (I16 b) = I16 (a * b)
        mul' (I16 a) (I32 b) = I32 (fromIntegral a * b)
        mul' (I16 a) (F b) = F (fromIntegral a * b)
        mul' (I32 a) (I8 b) = I32 (a * fromIntegral b)
        mul' (I32 a) (I16 b) = I32 (a * fromIntegral b)
        mul' (I32 a) (I32 b) = I32 (a * fromIntegral b)
        mul' (I32 a) (F b) = F (fromIntegral a * b)
        mul' (F a) (I8 b) = F (a * fromIntegral b)
        mul' (F a) (I16 b) = F (a * fromIntegral b)
        mul' (F a) (I32 b) = F (a * fromIntegral b)
        mul' (F a) (F b) = F (a * b)
        mul' _ _ = error "invalid types for mul"

evalOp Div = do
        a <- pop
        b <- pop
        push (b `div'` a)
    where
        div' (I8 a) (I8 b) = I8 (a `div` b)
        div' (I8 a) (I16 b) = I16 (fromIntegral a `div` b)
        div' (I8 a) (I32 b) = I32 (fromIntegral a `div` b)
        div' (I8 a) (F b) = F (fromIntegral a / b)
        div' (I16 a) (I8 b) = I16 (a `div` fromIntegral b)
        div' (I16 a) (I16 b) = I16 (a `div` b)
        div' (I16 a) (I32 b) = I32 (fromIntegral a `div` b)
        div' (I16 a) (F b) = F (fromIntegral a / b)
        div' (I32 a) (I8 b) = I32 (a `div` fromIntegral b)
        div' (I32 a) (I16 b) = I32 (a `div` fromIntegral b)
        div' (I32 a) (I32 b) = I32 (a `div` fromIntegral b)
        div' (I32 a) (F b) = F (fromIntegral a / b)
        div' (F a) (I8 b) = F (a / fromIntegral b)
        div' (F a) (I16 b) = F (a / fromIntegral b)
        div' (F a) (I32 b) = F (a / fromIntegral b)
        div' (F a) (F b) = F (a / b)
        div' _ _ = error "invalid types for Div"

evalOp Mod = do
        a <- pop
        b <- pop
        push (b `mod'` a)
    where
        mod' (I8 a) (I8 b) = I8 (a `mod` b)
        mod' (I8 a) (I16 b) = I16 (fromIntegral a `mod` b)
        mod' (I8 a) (I32 b) = I32 (fromIntegral a `mod` b)
        mod' (I16 a) (I8 b) = I16 (a `mod` fromIntegral b)
        mod' (I16 a) (I16 b) = I16 (a `mod` b)
        mod' (I16 a) (I32 b) = I32 (fromIntegral a `mod` b)
        mod' (I32 a) (I8 b) = I32 (a `mod` fromIntegral b)
        mod' (I32 a) (I16 b) = I32 (a `mod` fromIntegral b)
        mod' (I32 a) (I32 b) = I32 (a `mod` b)
        mod' _ _ = error "invalid types for mod"

evalOp Shl = do
        a <- pop
        b <- pop
        push (b `shl'` a)
    where
        shl' (I8 a) (I8 b) = I8 (a `shiftL` fromIntegral b)
        shl' (I8 a) (I16 b) = I8 (a `shiftL` fromIntegral b)
        shl' (I8 a) (I32 b) = I8 (a `shiftL` fromIntegral b)
        shl' (I16 a) (I8 b) = I16 (a `shiftL` fromIntegral b)
        shl' (I16 a) (I16 b) = I16 (a `shiftL` fromIntegral b)
        shl' (I16 a) (I32 b) = I16 (a `shiftL` fromIntegral b)
        shl' (I32 a) (I8 b) = I32 (a `shiftL` fromIntegral b)
        shl' (I32 a) (I16 b) = I32 (a `shiftL` fromIntegral b)
        shl' (I32 a) (I32 b) = I32 (a `shiftL` fromIntegral b)
        shl' _ _ = error "invalid types for shl"

evalOp Shr = do
        a <- pop
        b <- pop
        push (b `shr'` a)
    where
        shr' (I8 a) (I8 b) = I8 (a `shiftR` fromIntegral b)
        shr' (I8 a) (I16 b) = I8 (a `shiftR` fromIntegral b)
        shr' (I8 a) (I32 b) = I8 (a `shiftR` fromIntegral b)
        shr' (I16 a) (I8 b) = I16 (a `shiftR` fromIntegral b)
        shr' (I16 a) (I16 b) = I16 (a `shiftR` fromIntegral b)
        shr' (I16 a) (I32 b) = I16 (a `shiftR` fromIntegral b)
        shr' (I32 a) (I8 b) = I32 (a `shiftR` fromIntegral b)
        shr' (I32 a) (I16 b) = I32 (a `shiftR` fromIntegral b)
        shr' (I32 a) (I32 b) = I32 (a `shiftR` fromIntegral b)
        shr' _ _ = error "invalid types for shr"

evalOp Or = or' <$> pop <*> pop >>= push
    where
        or' (I8 a) (I8 b) = I8 (a .|. b)
        or' (I8 a) (I16 b) = I16 (fromIntegral a .|. b)
        or' (I8 a) (I32 b) = I32 (fromIntegral a .|. b)
        or' (I16 a) (I8 b) = I16 (a .|. fromIntegral b)
        or' (I16 a) (I16 b) = I16 (a .|. b)
        or' (I16 a) (I32 b) = I32 (fromIntegral a .|. b)
        or' (I32 a) (I8 b) = I32 (a .|. fromIntegral b)
        or' (I32 a) (I16 b) = I32 (a .|. fromIntegral b)
        or' (I32 a) (I32 b) = I32 (a .|. b)
        or' _ _ = error "invalid types for or"

evalOp And = and' <$> pop <*> pop >>= push
    where
        and' (I8 a) (I8 b) = I8 (a .&. b)
        and' (I8 a) (I16 b) = I16 (fromIntegral a .&. b)
        and' (I8 a) (I32 b) = I32 (fromIntegral a .&. b)
        and' (I16 a) (I8 b) = I16 (a .&. fromIntegral b)
        and' (I16 a) (I16 b) = I16 (a .&. b)
        and' (I16 a) (I32 b) = I32 (fromIntegral a .&. b)
        and' (I32 a) (I8 b) = I32 (a .&. fromIntegral b)
        and' (I32 a) (I16 b) = I32 (a .&. fromIntegral b)
        and' (I32 a) (I32 b) = I32 (a .&. b)
        and' _ _ = error "invalid types fand and"

evalOp Cmp = do
        a <- pop
        b <- pop
        push (b `sub'` a)
    where
        sub' (I8 a) (I8 b) = I32 (fromIntegral a - fromIntegral b)
        sub' (I8 a) (I16 b) = I32 (fromIntegral a - fromIntegral b)
        sub' (I8 a) (I32 b) = I32 (fromIntegral a - fromIntegral b)
        sub' (I8 a) (F b) = F (fromIntegral a - b)
        sub' (I16 a) (I8 b) = I32 (fromIntegral a - fromIntegral b)
        sub' (I16 a) (I16 b) = I32 (fromIntegral a - fromIntegral b)
        sub' (I16 a) (I32 b) = I32 (fromIntegral a - b)
        sub' (I16 a) (F b) = F (fromIntegral a - b)
        sub' (I32 a) (I8 b) = I32 (a - fromIntegral b)
        sub' (I32 a) (I16 b) = I32 (a - fromIntegral b)
        sub' (I32 a) (I32 b) = I32 (a - fromIntegral b)
        sub' (I32 a) (F b) = F (fromIntegral a - b)
        sub' (F a) (I8 b) = F (a - fromIntegral b)
        sub' (F a) (I16 b) = F (a - fromIntegral b)
        sub' (F a) (I32 b) = F (a - fromIntegral b)
        sub' (F a) (F b) = F (a - b)
        sub' _ _ = error "invalid types for sub"

