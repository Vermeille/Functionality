module Main where

import VM
import Eval
import Opcodes
import Control.Lens
import Control.Monad.State

funMain :: Function
funMain = newFun "Main" [] $
                [ Push (Pushimm (I8 1))
                , Branch (Beq 3)
                , Push (Pushimm (I16 666))
                , Branch Ret ]


main :: IO ()
main = do
        let vm = newVM [funMain]
        let res = runState runVM vm
        print $ res ^. _2 . stack

