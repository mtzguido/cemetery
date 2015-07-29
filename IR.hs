-- Cemetery intermediate representation.
--
-- It's similar to what you would have in a regular machine-code
-- compiler, except that we don't need to completely flatten it since
-- we're compiling into a high-level language which supports branching
-- and looping.
--
-- For now, this IR only represents functions that deal with integers.

module IR where

import Common
import Data.Word
import Data.List

type IR = [Unit]

data Type = Int | Bool | ArrT Type
          | Bits
  deriving (Eq, Show)

data Unit = FunDef Funtype Block
          | Decl Decl
  deriving (Eq, Show)

data Funtype = Funtype { name :: String,
                         args :: [(String, Type)],
                         ret :: Type }
  deriving (Eq, Show)

data BinOp = Plus | Minus | Div | Prod | Eq | Mod | And | Or
           | Band | Bor | BConcat | Xor
           | LShift | RShift | LRot | RRot
           | Le | Lt | Gt | Ge
           | ModPlus -- Bits addition modulo size
           | BitEq
  deriving (Eq, Show)

data UnOp = Neg | Not | Bnot
  deriving (Eq, Show)

data LValue = LVar String
            | Temp Int
            | Builtin Builtin
  deriving (Eq, Show, Ord)

data Builtin = Permute
             | Length
             | ToInt
             | ToBits
             | Zero
  deriving (Eq, Show, Ord)

data Expr = ConstInt   Int
          | ConstBool  Bool
          | BinOp      BinOp Expr Expr
          | UnOp       UnOp Expr
          | Call       LValue [Expr]
          | LV         LValue
          | Arr        [Expr]
          | Slice      Expr Expr Expr
          | Access     Expr Expr
          | ConstBits  [Int] Int
          | Copy       Expr
  deriving (Eq, Show)

type Block = ([Decl], Stmt)

data Decl = DeclLocal  LValue Type
          | DeclGlobal String Type Expr
  deriving (Eq, Show)

data Stmt = Assign      LValue Expr
          | Return      Expr
          | Seq         Stmt Stmt
          | Skip                         -- Simply discard this
          | If          Expr Block Block -- C-like semantics
          | For         LValue Expr Expr Block
          | Error       String
          | Free        LValue
  deriving (Eq, Show)

sseq Skip r = r
sseq l Skip = l
sseq l r = Seq l r

sfold = foldl sseq Skip
