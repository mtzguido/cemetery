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

data Type = Int | Bool
  deriving (Eq, Show)

data Unit = FunDef Funtype Block
          | Decl Decl
  deriving (Eq, Show)

data Funtype = Funtype { name :: String,
                         args :: [(String, Type)],
                         ret :: Type }
  deriving (Eq, Show)

data BinOp = Plus | Minus | Div | Prod | Eq | Mod | And | Or
  deriving (Eq, Show)

data UnOp = Neg | Not
  deriving (Eq, Show)

data LValue = LVar String
  deriving (Eq, Show)

data Expr = ConstInt   Int
          | ConstBool  Bool
          | BinOp      BinOp Expr Expr
          | UnOp       UnOp Expr
          | Call       String [Expr]
          | Var        String
          | ESeq       Stmt Expr
  deriving (Eq, Show)

type Block = ([Decl], Stmt)

data Decl = DeclareVar  String Type
  deriving (Eq, Show)

data Stmt = Assign      LValue Expr
          | Return      Expr
          | Seq         Stmt Stmt
          | Skip                         -- Simply discard this
          | Expr        Expr
          | If          Expr Block Block -- C-like semantics
  deriving (Eq, Show)
