module Opcodes where

import Data.Int

data Opcode = Ar Arith
            | Ld LdOp
            | Push PushOp
            | Branch BranchOp

data Arith = Add
           | Sub
           | Mul
           | Div
           | Mod
           | Shl
           | Shr
           | Or
           | And
           | Cmp
           deriving (Show)

data LdOp = Ldloc Int
          | Ldloca Int
          | Ldarg Int
          | Lda Int
          | Dup
          deriving (Show)

data PushOp = Pushimm Value
            deriving (Show)

data BranchOp   = Beq Int
                | Bneq Int
                | Blt Int
                | Bltq Int
                | Bgt Int
                | Bgtq Int
                | Jmp Int
                | Ret
                | Call Int
                | BreakPoint
                deriving (Show)

data Value  = I8 !Int8
            | I16 !Int16
            | I32 !Int32
            | F !Float
            | Ptr !(Int, VarPlace, Int)
            deriving (Show)

data VarPlace = Local | Arg | Temp | Heap
             deriving (Show)

-- Almost the same as Value. I don't like it
data VarType = TyI8 | TyI16 | TyI32 | TyF | TyPtr
                deriving (Show)

instance Show Opcode where
        show (Ar a) = show a
        show (Ld ld) = show ld
        show (Push p) = show p
        show (Branch b) = show b

