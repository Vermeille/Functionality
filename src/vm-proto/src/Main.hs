module Main where

import VM
import Asm
import Eval
import Opcodes
import Control.Lens
import Control.Monad.State

vm :: Memory
vm = newVM [funMain] [struct]
    where
        funMain = newFun "Main" [] TyI16 [] $
                        [ push_ (I16 666)
                        , push_ (I8 42)
                        , construct_ 0 0
                        , brk_
                        , ret_ ]
        struct = TyUnion "Simple" [Ctor "SCtor" [TyI8, TyI16] ]

main :: IO ()
main = do
        let res = runState runVM vm
        print $ res ^. _2 . stack

