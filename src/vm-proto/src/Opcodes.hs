{-|
module: Opcodes

This module defines all instructions (the name "Opcode" might be a little
misleading), as well as types and values. You may want to read about the VM
design explained in the module VM first.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Opcodes where

import Data.Int
import Control.Lens
import Data.List (intercalate)

-- | For now, we have four categories of instructions
data Opcode = Ar Arith
            | Ld LdOp
            | St StOp
            | Push PushOp
            | Branch BranchOp

-- | Basic arithmetical operations
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

-- | loading instructions
data LdOp = Ldloc Int           -- ^ Push the nth local variable on the stack
          | Ldloca Int          -- ^ Push the nth local var's address
          | Ldarg Int           -- ^ Push the nth argument
          | Ldarga Int          -- ^ Push the nth argument address
          | Lda Int             -- ^ FIXME: Still obscure.
          | Ldslot Int          -- ^ Push the nth member of the union on ToS
          | Ldslota Int         -- ^ Push the nth member's addr of union on ToS
          | Dup                 -- ^ Duplicate the ToS
          | Construct Int Int   -- ^ Construct the nth Union with the mth ctor
                                --   taking members on the stack
          deriving (Show)

data StOp   = Stloc Int
            | Stlocat Int
            | Starg Int
            | Stargate Int
            deriving (Show)

data PushOp = Pushimm Value     -- ^ Push an immediate value
            deriving (Show)

data BranchOp   = Beq Int       -- ^ Branch Equal
                | Bneq Int      -- ^ Branch Non-equal
                | Blt Int       -- ^ Branch less than
                | Bltq Int      -- ^ Branch less than or equal
                | Bgt Int       -- ^ Branch greater than
                | Bgtq Int      -- ^ Branch greater than or equal
                | Jmp Int       -- ^ Inconditionnal jump
                | Ret           -- ^ Exit the function and push the ToS on the
                                --   previous stack frame
                | Call Int      -- ^ Call a function at address
                | BreakPoint    -- ^ Debugger entry, for now, print the VM
                | Match Int Int -- ^ if ToS was built with ctor arg1, then jump
                                --   to arg2
                deriving (Show)

-- Almost the same as Value. I don't like it
data VarType = TyI8 | TyI16 | TyI32 | TyF | TyPtr | UnionId Int
                deriving (Show)

-- | Indexes values through the "MMU". It's just an address and a type
data ValueIx = ValueIx !VarType !Int

instance Show ValueIx where
        show (ValueIx ty idx) = show ty ++ " [" ++ show idx ++ "]"

-- | The values the VM is able to deal with.
data Value  = I8  !Int8
            | I16 !Int16
            | I32 !Int32
            | F   !Float
            | Ptr !Int -- an index in the MMU
            | Union !Int
            deriving (Show)

makeLenses ''Value

-- | A tagged union.
data TyUnion = TyUnion { _unionName :: String, _ctors :: [Ctor] }

-- | A Data constructor for a tagged union
data Ctor = Ctor { _ctorName :: String, _ctorMembers :: [VarType] }

makeLenses ''TyUnion
makeLenses ''Ctor

instance Show Opcode where
        show (Ar a) = show a
        show (Ld ld) = show ld
        show (Push p) = show p
        show (Branch b) = show b
        show (St _) = undefined -- FIXME

instance Show TyUnion where
    show (TyUnion nm ctors') = "data " ++ nm ++ " = "
        ++ (intercalate "\n  | " . map show $ ctors')

instance Show Ctor where
    show (Ctor nm membs) = nm ++ " " ++ (unwords . map show $ membs)

-- | returns the first parameters if the type matches with the second
--   parameter. Throw an exception otherwise.
typeCheck :: Value -> VarType -> Value
typeCheck a@(I8 _) TyI8 = a
typeCheck a@(I16 _) TyI16 = a
typeCheck a@(I32 _) TyI32 = a
typeCheck a@(F _) TyF = a
typeCheck _ _ = error "typechecking failed"

