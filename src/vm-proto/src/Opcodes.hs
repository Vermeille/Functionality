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
          | Lda Int             -- ^ FIXME: Still obscure.
          | Ldslot Int          -- ^ Push the nth member of the union on ToS
          | Ldslota Int         -- ^ Push the nth member's addr of union on ToS
          | Dup                 -- ^ Duplicate the ToS
          | Construct Int Int   -- ^ Construct the nth Union with the mth ctor
                                --   taking members on the stack
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

-- | The values the VM is able to deal with.
data Value  = I8 !Int8
            | I16 !Int16
            | I32 !Int32
            | F !Float
            | Ptr !(VarPlace, [Int]) -- FIXME: I'm not abstract enough!
            | Union {_ctorId :: !Int, _unionValues :: [Value] }
            deriving (Show)

-- | Where is the var? Used by Ptr.
data VarPlace = Local | Arg | Temp | Heap
             deriving (Show)

makeLenses ''Value

-- Almost the same as Value. I don't like it
data VarType = TyI8 | TyI16 | TyI32 | TyF | TyPtr | UnionId Int
                deriving (Show)

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

