module Opcodes where

import Data.Int

data Opcode = Ar Arith
            | Ld LdOp
            | Push PushOp
            | Branch BranchOp
            deriving (Show)

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
                deriving (Show)

data Value  = I8 !Int8
            | I16 !Int16
            | I32 !Int32
            | F !Float
            | Ptr !(Int, VarType, Int)
            deriving (Show)

data VarType = Local | Arg | Temp
             deriving (Show)
