module Main where

import VM
import Eval
import Opcodes
import Control.Lens
import Control.Monad.State

funMain :: Function
funMain = newFun "Main" [] (I16 0) [] $
                [ Push (Pushimm (I16 1))
                , Push (Pushimm (I16 666))
                , Branch (Call 1)
                , Branch Ret ]

funAdd :: Function
funAdd = newFun "Add" [I16 0, I16 0] (I16 0) []
                [ Ld (Ldarg 0)
                , Ld (Ldarg 1)
                , Ar Add
                , Branch Ret]


main :: IO ()
main = do
        let vm = newVM [funMain, funAdd]
        let res = runState runVM vm
        print $ res ^. _2 . stack

