{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens
import Control.Applicative
import Data.Maybe (fromJust, fromMaybe)

data Value  = I8 Int
            | I16 Int
            | I32 Int
            | F Float
            | Ptr (Int, VarType, Int)
            deriving (Show)

data VarType = Local | Arg | Temp
             deriving (Show)

data FunEnv = FunEnv { _loc :: [Value], _args :: [Value], _temps :: [Value] }
            deriving (Show)
data Memory = Memory { _stack :: S.Seq FunEnv, _code :: V.Vector Opcode, _pc :: Int }
            deriving (Show)

data Arith = Add | Sub | Mul | Div
            deriving (Show)
data LdOp = Ldloc Int | Ldloca Int | Ldarg Int | Lda Int
            deriving (Show)

data PushOp = Pushimm Value
            deriving (Show)

data Opcode = Ar Arith | Ld LdOp | Push PushOp
            deriving (Show)

makeLenses ''FunEnv
makeLenses ''Memory

newStackFrame args = FunEnv { _loc = [], _args = args, _temps = [] }
newVM code = Memory { _stack = S.fromList [newStackFrame []], _code = V.fromList code, _pc = 0 }

topFun = stack . _last
topTemp = temps . _head
tos = topFun . topTemp

takeVar Local = _loc
takeVar Arg = _args
takeVar Temp = _temps

push :: Value -> State Memory ()
push v = state $ \mem -> ((), mem & stack . _last . temps  %~ (v:))

pop :: State Memory Value
pop = state $ \mem -> (topVal mem, mem & stack . _last . temps %~ tail)
    where
        topVal mem = fromMaybe (error "poping an empty stack") $ mem ^? topFun . topTemp

evalLd :: LdOp -> State Memory ()
evalLd (Ldloc n) = preuse (stack . _last . loc . ix n) >>= push . fromJust
evalLd (Ldloca n) = do
            stackLevel <- uses stack S.length
            push (Ptr (stackLevel, Local, n))
evalLd (Ldarg n) = preuse (stack . _last . args . ix n) >>= push . fromJust
evalLd (Lda n) =
        do
            Ptr (s', ty, n) <- pop
            val <- preuse (stack . ix s' . to (takeVar ty) . ix n)
            push (fromJust val)

evalPush (Pushimm v) = push v

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

eval (Ar a) = do evalOp a
eval (Ld ld) = do evalLd ld
eval (Push p) = do evalPush p

runVM = do
        idx <- use pc
        len <- uses code V.length
        if idx == len then
            return ()
        else do
            opcode <- preuse (code . ix idx)
            eval $ fromMaybe (error "invalid pc") opcode
            pc += 1
            runVM

main = do
        let vm = newVM [Push (Pushimm (I8 1)), Push (Pushimm (I8 2)), Ar Add]
        let res = runState runVM vm
        print $ res ^. _2 . stack
