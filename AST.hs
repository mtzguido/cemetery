module AST where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.Maybe

type VarName = String

data Type = Int
          | Bool
          | String
          | Bytes
          | Fun [Type] Type
          deriving (Eq, Show)

cmtTypeTable = [("int", Int), ("bool", Bool), ("string", String),
                ("bytes", Bytes)]

data BinOp = Plus | Minus | Div | Prod | Eq | Mod
           | Xor
          deriving (Eq, Show)

data UnOp = NegateNum
          deriving (Eq, Show)

data Expr = ConstInt Int
          | BinOp BinOp Expr Expr
          | UnOp UnOp Expr
          | Call VarName [Expr]
          | Var VarName
          | ConstStr String
          deriving (Eq, Show)

data Stmt = Skip
          | Assign VarName Expr
          | If Expr Stmt Stmt
          | Seq Stmt Stmt
          | Return Expr
          | Decl Decl
          deriving (Eq, Show)

data Decl = DeclareAuto String
          | Declare String Expr
          | DeclareAutoT String Type
          | DeclareT String Expr Type
          | Const String Expr
          | External String Type
          | Struct
          deriving (Eq, Show)

data FunDecl = FunDecl {
    name :: String,
    ret :: Type,
    args :: [(String, Type)],
    body :: Stmt
} deriving (Eq, Show)

type Prog = ([Decl], [FunDecl])

type NameEnv = M.Map VarName Type
