module CLang where

import Data.Word

-- C Lanuage definition, but with some of our
-- types that will later get translated by the C
-- Generator

data Prog = Prog { includes :: [String],
                   units :: [Unit]
                 }
  deriving (Eq, Show)

data Mods = Extern | Const | Static
  deriving (Eq, Show)

data Unit = Decl Decl
          | FunDecl Funtype
          | FunDef Funtype Block
  deriving (Eq, Show)

type Block = ([Decl], Stmt)

data Decl = VarDecl String Type (Maybe Expr) [Mods]
  deriving (Eq, Show)

data Funtype = Funtype { name :: String,
                         args :: [(String, Type)],
                         mods :: [Mods],
                         ret :: Type }
  deriving (Eq, Show)

data LValue = LVar String
            | Access Expr Expr
  deriving (Eq, Show)

data Stmt = If Expr Block Block
          | Seq Stmt Stmt
          | Return Expr
          | Skip
          | For Expr Expr Expr Block
          | Expr Expr
          | Comment String
  deriving (Eq, Show)

data Expr = BinOp BinOp Expr Expr
          | UnOp UnOp Expr
          | ConstInt Int
          | ConstFloat Double
          | Call String [Expr]
          | LV LValue
          | ConstBool Bool
          | Arr [Expr]
          | ConstStr String
          | StructVal [(String, Expr)]
  deriving (Eq, Show)

data BinOp = Plus | Minus | Div | Prod
           | Eq | Neq | Mod | And | Or
           | Lt | Gt | Le | Ge
           | Band | Bor | Xor
           | Member -- "->"
           | Assign
  deriving (Eq, Show)

data UnOp = NegateNum | Not | Bnot | Address
  deriving (Eq, Show)

data Type = Int | Bool | ArrT Type | Custom String
          | UChar
  deriving (Eq, Show)

sseq Skip s = s
sseq s Skip = s
sseq s t = Seq s t
