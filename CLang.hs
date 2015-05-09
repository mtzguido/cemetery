module CLang where

import Data.Word

-- C Lanuage definition, but with some of our
-- types that will later get translated by the C
-- Generator

type Prog = [Unit]

data VarModifiers = Extern | Const | Static
  deriving (Eq, Show)

data Unit = Decl Decl
          | FunDecl Funtype
          | FunDef Funtype Block
  deriving (Eq, Show)

type Block = ([Decl], Stmt)

data Decl = VarDecl String Type (Maybe Expr) [VarModifiers]
  deriving (Eq, Show)

data Funtype = Funtype { name :: String,
                         args :: [(String, Type)],
                         ret :: Type }
  deriving (Eq, Show)

data Stmt = Assign String Expr
          | If Expr Block Block
          | Seq Stmt Stmt
          | Return Expr
          | Skip
  deriving (Eq, Show)

data Expr = BinOp BinOp Expr Expr
          | UnOp UnOp Expr
          | ConstInt Int
          | ConstFloat Double
          | Call String [Expr]
          | Var String
          | ConstBool Bool
          | ConstStr String
          | ConstArr [Word8]
  deriving (Eq, Show)

data BinOp = Plus | Minus | Div | Prod
           | Eq | Mod
  deriving (Eq, Show)

data UnOp = NegateNum
  deriving (Eq, Show)

data Type = Int | Void | Double | Bool | String
          | Ptr Type | Fun Funtype | CmtBuf
  deriving (Eq, Show)

sseq Skip s = s
sseq s Skip = s
sseq s t = Seq s t
