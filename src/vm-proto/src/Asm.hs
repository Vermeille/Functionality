module Asm where

import Opcodes

{-| The purpose of this module is only to make a nicer syntax when writing
  in-source asm (ie: for testing) -}

add_ :: Opcode
add_ = Ar Add

sub_ :: Opcode
sub_ = Ar Sub

mul_ :: Opcode
mul_ = Ar Mul

div_ :: Opcode
div_ = Ar Mod

shl_ :: Opcode
shl_ = Ar Shl

shr_ :: Opcode
shr_ = Ar Shr

or_ :: Opcode
or_  = Ar Or

and_ :: Opcode
and_ = Ar And

cmp_ :: Opcode
cmp_ = Ar Cmp

ldloc_ :: Int -> Opcode
ldloc_ = Ld . Ldloc

ldloca_ :: Int -> Opcode
ldloca_ = Ld . Ldloca

ldarg_ :: Int -> Opcode
ldarg_ = Ld . Ldarg

lda_ :: Int -> Opcode
lda_ = Ld . Lda

ldslot_ :: Int -> Opcode
ldslot_ = Ld . Ldslot

ldslota_ :: Int -> Opcode
ldslota_ = Ld . Ldslota

dup_ :: Opcode
dup_ = Ld Dup

construct_ :: Int -> Int -> Opcode
construct_ unionid ctor = Ld (Construct unionid ctor)

beq_ :: Int -> Opcode
beq_ = Branch . Beq

bneq_ :: Int -> Opcode
bneq_ = Branch . Bneq

blt_ :: Int -> Opcode
blt_ = Branch . Blt

bltq_ :: Int -> Opcode
bltq_ = Branch . Bltq

bgt_ :: Int -> Opcode
bgt_ = Branch . Bgt

bgtq_ :: Int -> Opcode
bgtq_ = Branch . Bgtq

jmp_ :: Int -> Opcode
jmp_ = Branch . Jmp

ret_ :: Opcode
ret_ = Branch Ret

call_ :: Int -> Opcode
call_ = Branch . Call

brk_ :: Opcode
brk_ = Branch BreakPoint

match_ :: Int -> Int -> Opcode
match_ cid dst= Branch (Match cid dst)

push_ :: Value -> Opcode
push_ = Push . Pushimm
