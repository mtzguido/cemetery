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
           | Band | Bor | BConcat
  deriving (Eq, Show)

data UnOp = Neg | Not | Bnot
  deriving (Eq, Show)

data LValue = LVar String
            | Temp Int
  deriving (Eq, Show)

data Expr = ConstInt   Int
          | ConstBool  Bool
          | BinOp      BinOp Expr Expr
          | UnOp       UnOp Expr
          | Call       String [Expr]
          | LV         LValue
          | Arr        [Expr]
          | Slice      Expr Expr Expr
          | Access     Expr Expr
  deriving (Eq, Show)

type Block = ([Decl], Stmt)

data Decl = DeclareVar    String Type
          | DeclareGlobal String Type Expr
          | DeclareTemp   Int    Type
  deriving (Eq, Show)

data Stmt = Assign      LValue Expr
          | Return      Expr
          | Seq         Stmt Stmt
          | Skip                         -- Simply discard this
          | If          Expr Block Block -- C-like semantics
  deriving (Eq, Show)
