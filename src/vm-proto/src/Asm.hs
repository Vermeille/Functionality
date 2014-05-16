module Asm where

import Opcodes

{- The purpose of this module is only to make a nicer syntax when writing
  in-source asm (ie: for testing) -}

add_ = Ar Add
sub_ = Ar Sub
mul_ = Ar Mul
div_ = Ar Mod
shl_ = Ar Shl
shr_ = Ar Shr
or_  = Ar Or
and_ = Ar And
cmp_ = Ar Cmp

ldloc_ = Ld . Ldloc
ldloca_ = Ld . Ldloca
ldarg_ = Ld . Ldarg
lda_ = Ld . Lda
dup_ = Ld Dup

beq_ = Branch . Beq
bneq_ = Branch . Bneq
blt_ = Branch . Blt
bltq_ = Branch . Bltq
bgt_ = Branch . Bgt
bgtq_ = Branch . Bgtq
jmp_ = Branch . Jmp
ret_ = Branch Ret
call_ = Branch . Call
brk_ = Branch BreakPoint

push_ = Push . Pushimm
