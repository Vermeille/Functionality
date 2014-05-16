{-# LANGUAGE TemplateHaskell #-}
module Opcodes where

import Data.Int
import Control.Lens
import Data.List (intercalate)

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
          | Construct Int Int
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
            | Union !Int [Value]
            deriving (Show)

data VarPlace = Local | Arg | Temp | Heap
             deriving (Show)

-- Almost the same as Value. I don't like it
data VarType = TyI8 | TyI16 | TyI32 | TyF | TyPtr | UnionId Int
                deriving (Show)

data TyUnion = TyUnion { _unionName :: String, _ctors :: [Ctor] }
data Ctor = Ctor { _ctorName :: String, _ctorMembers :: [VarType] }

makeLenses ''TyUnion
makeLenses ''Ctor

instance Show Opcode where
        show (Ar a) = show a
        show (Ld ld) = show ld
        show (Push p) = show p
        show (Branch b) = show b

instance Show TyUnion where
    show (TyUnion nm ctors') = "data " ++ nm ++ " = "
        ++ (intercalate "\n  | " . map show $ ctors')

instance Show Ctor where
    show (Ctor nm membs) = nm ++ " " ++ (unwords . map show $ membs)

typeCheck :: Value -> VarType -> Value
typeCheck a@(I8 _) TyI8 = a
typeCheck a@(I16 _) TyI16 = a
typeCheck a@(I32 _) TyI32 = a
typeCheck a@(F _) TyF = a
typeCheck _ _ = error "typechecking failed"

