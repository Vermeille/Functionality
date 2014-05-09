module Opcodes where

data Opcode = Ar Arith | Ld LdOp | Push PushOp
            deriving (Show)

data Arith = Add
           | Sub
           | Mul
           | Div
           deriving (Show)

data LdOp = Ldloc Int
          | Ldloca Int
          | Ldarg Int
          | Lda Int
          deriving (Show)

data PushOp = Pushimm Value
            deriving (Show)

data Value  = I8 Int
            | I16 Int
            | I32 Int
            | F Float
            | Ptr (Int, VarType, Int)
            deriving (Show)

data VarType = Local | Arg | Temp
             deriving (Show)

