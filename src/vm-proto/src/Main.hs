module Main where

import VM
import Asm
import Eval
import Opcodes
import Control.Lens
import Control.Monad.State

vm :: VM
vm = newVM [vmMain, vmAdd] []
    where
        vmMain = newFun "Main" [] TyI8 []
                [ push_ (I8 2)
                , push_ (I8 4)
                , call_ 1
                , ret_]
        vmAdd = newFun "Add" [TyI8, TyI8] TyI8 []
                [ ldarg_ 0
                , ldarg_ 1
                , add_
                , ret_ ]
-- | Main, for now, just execute the code and show what the vm's Main returns
main :: IO ()
{-
main = do
        let res = runState runVM vm
        print $ res ^. _2 . stack
-}
main = vmLoop vm
        where
            vmLoop vm' = do
                let res = runState stepVM vm'
                print $! res ^. _2
                if ((== -1) . (^. _2 . pc . _1)) res
                    then return ()
                    else vmLoop $ res ^. _2

