module CLang where

type Prog = [Unit]

data VarModifiers = Extern | Const
  deriving (Eq, Show)

data Unit = VarDecl String Type [VarModifiers]
          | FunDecl Funtype
          | FunDef Funtype Stmt
          | UnitStub
  deriving (Eq, Show)

data Funtype = Funtype { name :: String,
                         args :: [(String, Type)],
                         ret :: Type }
  deriving (Eq, Show)

data Stmt = Assign String Expr
          | If Expr Stmt Stmt
          | Seq Stmt Stmt
          | Return Expr
          | Skip
  deriving (Eq, Show)

data Expr = BinOp BinOp Expr Expr
          | UnOp UnOp Expr Expr
          | ConstInt Int
          | ConstFloat Double
          | Call String [Expr]
          | Var String
  deriving (Eq, Show)

data BinOp = Plus | Minus | Div | Prod
           | Eq | Mod
  deriving (Eq, Show)

data UnOp = NegateNum
  deriving (Eq, Show)

data Type = Int | Void | Double
  deriving (Eq, Show)
