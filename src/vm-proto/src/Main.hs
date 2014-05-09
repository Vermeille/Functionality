module Main where

import VM
import Eval
import Opcodes
import Control.Lens
import Control.Monad.State

main = do
        -- let vm = newVM [Push (Pushimm (I8 1)), Push (Pushimm (I8 2)), Ar Add, Push (Pushimm (F 666)), Ar Div]
        let vm = newVM [Push (Pushimm (I8 1)), Branch (Beq 3), Push (Pushimm (I16 666))]
        let res = runState runVM vm
        print $ res ^. _2 . stack

