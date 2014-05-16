module Main where

import VM
import Asm
import Eval
import Opcodes
import Control.Lens
import Control.Monad.State

funMain :: Function
funMain = newFun "Main" [] TyI16 [] $
                [ push_ (I16 1)
                , push_ (I16 666)
                , call_ 1
                , ret_ ]

funAdd :: Function
funAdd = newFun "Add" [TyI16, TyI16] TyI16 []
                [ ldarg_ 0
                , ldarg_ 1
                , add_
                , ret_ ]


main :: IO ()
main = do
        let vm = newVM [funMain, funAdd] []
        let res = runState runVM vm
        print $ res ^. _2 . stack

