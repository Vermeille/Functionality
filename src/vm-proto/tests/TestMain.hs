{-# LANGUAGE TemplateHaskell #-}
module Main where

import VM
import Eval
import Opcodes
import Data.Int
import Data.Bits
import Branching
import Control.Lens
import Test.QuickCheck.All
import Control.Monad.State
import Test.QuickCheck hiding ((.&.))

testOp op opcode a b =
        let res = runState runVM vm in
        whenFail (putStrLn ("my out: " ++ show (res ^? _2.tos)
            ++ " in: " ++ show a ++ " " ++ show b
            ++ " expected: " ++ show (a `op` b))) $
        case res ^? _2 . tos of
            Just val -> valToInt val == fromIntegral (a `op` b)
            Nothing ->  False
        where
            vm = newVM [code]
            code = newFun "Main" []
                    [ Push (Pushimm (I8 a))
                    , Push (Pushimm (I8 b))
                    , Ar opcode
                    , Branch Ret]

prop_add = testOp (+) Add
prop_sub = testOp (-) Sub
prop_mul = testOp (*) Mul
prop_div a b = b /= 0 ==> testOp div Div a b
prop_mod a b = b /= 0 ==> testOp mod Mod a b
prop_shl = testOp (\x y -> x `shiftL` fromIntegral y) Shl
prop_shr = testOp (\x y -> x `shiftR` fromIntegral y) Shr
prop_or = testOp (.|.) Or
prop_and = testOp (.&.) And

testBr op opcode a b =
        let res = runState runVM vm in
        whenFail (putStrLn ("my out: " ++ show (res ^? _2.tos)
            ++ " in: " ++ show a ++ " " ++ show b)) $
        case res ^? _2 . tos of
            Just val -> valToInt val == (if a `op` b then 1 else 0)
            Nothing ->  False
        where
            vm = newVM [code]
            code = newFun "Main" []
                    [ Push (Pushimm (I8 a)) -- 0
                    , Push (Pushimm (I8 b)) -- 1
                    , Ar Cmp                -- 2
                    , Branch (opcode 6)     -- 3
                    , Push (Pushimm (I8 0)) -- 4
                    , Branch Ret     -- 5
                    , Push (Pushimm (I8 1)) -- 6
                    , Branch (Ret)]    -- 7

prop_beq  a b = a - b < 5 ==> testBr (==) Beq a b
prop_bneq a b = a - b < 5 ==> testBr (/=) Bneq a b
prop_blt  a b = a - b < 5 ==> testBr (<) Blt a b
prop_bgt  a b = a - b < 5 ==> testBr (>) Bgt a b
prop_bltq a b = a - b < 5 ==> testBr (<=) Bltq a b
prop_bgtq a b = a - b < 5 ==> testBr (>=) Bgtq a b

main = $quickCheckAll
