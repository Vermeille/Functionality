{-# LANGUAGE TemplateHaskell #-}
module Main where

import VM
import Asm
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
            vm = newVM [code] []
            code = newFun "Main" [] TyI8 []
                    [ push_ (I8 a)
                    , push_ (I8 b)
                    , Ar opcode
                    , ret_ ]

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
            vm = newVM [code] []
            code = newFun "Main" [] TyI8 []
                    [ push_ (I8 a)      -- 0
                    , push_ (I8 b)      -- 1
                    , cmp_              -- 2
                    , Branch (opcode 6) -- 3
                    , push_ (I8 0)      -- 4
                    , ret_              -- 5
                    , push_ (I8 1)      -- 6
                    , ret_ ]            -- 7

prop_beq  a b = a - b < 5 ==> testBr (==) Beq a b
prop_bneq a b = a - b < 5 ==> testBr (/=) Bneq a b
prop_blt  a b = a - b < 5 ==> testBr (<) Blt a b
prop_bgt  a b = a - b < 5 ==> testBr (>) Bgt a b
prop_bltq a b = a - b < 5 ==> testBr (<=) Bltq a b
prop_bgtq a b = a - b < 5 ==> testBr (>=) Bgtq a b

prop_call a b =
        let res = runState runVM vm in
        whenFail (putStrLn ("my out: " ++ show (res ^? _2.tos)
            ++ " in: " ++ show a ++ " " ++ show b
            ++ " expected: " ++ show (a + b))) $
        case res ^? _2 . tos of
            Just val -> valToInt val == fromIntegral (a + b)
            Nothing ->  False
        where
            vm = newVM [vmMain, vmAdd] []
            vmMain = newFun "Main" [] TyI8 []
                    [ push_ (I8 a)
                    , push_ (I8 b)
                    , call_ 1
                    , ret_]
            vmAdd = newFun "Add" [TyI8, TyI8] TyI8 []
                    [ ldarg_ 0
                    , ldarg_ 1
                    , add_
                    , ret_ ]

prop_match = do
        a <- choose (0, 2)
        b <- choose (0, 2)
        return $! verify a b
        where
            verify a b = let res = runState runVM vm in
                whenFail (putStrLn ("my out: " ++ show (res ^? _2.tos)
                    ++ " in: " ++ show a ++ " " ++ show b
                    ++ " expected: " ++ show expected )) $
                case res ^? _2 . tos of
                    Just (I8 val) -> val == expected
                    Nothing ->  False
                where
                    vm = newVM [vmMain] [vmSStruct]

                    vmMain = newFun "Main" [] TyI8 []
                            [ push_ (I8 42)     -- 0
                            , construct_ 0 a    -- 1
                            , match_ b 5        -- 2
                            , push_ (I8 0)      -- 3
                            , jmp_ 6            -- 4
                            , push_ (I8 1)      -- 5
                            , ret_              -- 6
                            ]
                    vmSStruct = TyUnion "SStruct" [ Ctor "SCtor1" [TyI8]
                                                  , Ctor "SCtor2" [TyI8]
                                                  , Ctor "SCtor3" [TyI8] ]
                    expected = if a == b then 1 else 0

prop_ldslot = do
        a <- choose (0, 2)
        return $! verify a
        where
            verify a = let res = runState runVM vm in
                whenFail (putStrLn ("my out: " ++ show (res ^? _2.tos)
                    ++ " in: " ++ show a
                    ++ " expected: " ++ show a )) $
                case res ^? _2 . tos of
                    Just (I8 val) -> val == fromIntegral a
                    Nothing ->  False
                where
                    vm = newVM [vmMain] [vmSStruct]

                    vmMain = newFun "Main" [] TyI8 []
                            [ push_ (I8 2)      -- 0
                            , push_ (I8 1)      -- 1
                            , push_ (I8 0)      -- 2
                            , construct_ 0 0    -- 3
                            , ldslot_ a         -- 4
                            , ret_              -- 5
                            ]
                    vmSStruct = TyUnion "SStruct" [ Ctor "SCtor1"
                                                    [TyI8, TyI8, TyI8] ]

main = $quickCheckAll
