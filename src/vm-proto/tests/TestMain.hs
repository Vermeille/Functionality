{-# LANGUAGE TemplateHaskell #-}
module Main where

import VM
import Eval
import Opcodes
import Data.Int
import Data.Bits
import Branching
import Control.Lens
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.All
import Control.Monad.State

testOp op opcode a b =
        let res = runState runVM vm in
        whenFail (putStrLn ("my out: " ++ show (res ^? _2.tos)
            ++ " in: " ++ show a ++ " " ++ show b
            ++ " expected: " ++ show (a `op` b))) $
        case res ^? _2 . tos of
            Just val -> valToInt val == fromIntegral (a `op` b)
            Nothing ->  False
        where
            vm = newVM code
            code = [Push (Pushimm (I8 a)),
                   Push (Pushimm (I8 b)),
                   Ar opcode]

prop_add = testOp (+) Add
prop_sub = testOp (-) Sub
prop_mul = testOp (*) Mul
prop_div a b = b /= 0 ==> testOp div Div a b
prop_mod a b = b /= 0 ==> testOp mod Mod a b
prop_shl = testOp (\x y -> x `shiftL` fromIntegral y) Shl
prop_shr = testOp (\x y -> x `shiftR` fromIntegral y) Shr
prop_or = testOp (.|.) Or
prop_and = testOp (.&.) And

main = $quickCheckAll
