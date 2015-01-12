module AST where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.Maybe

type VarName = String

data Type = Int
          | Bool
          | String
          | Fun [Type] Type
          deriving (Eq, Show)

cmtTypeTable = [("int", Int), ("bool", Bool), ("string", String)]

data BinOp = Plus | Minus | Div | Prod | Eq
          deriving (Eq, Show)

cmtBinOpTable = [("+", Plus), ("-", Minus), ("*", Prod),
                 ("/", Div), ("==", Eq)]

data UnOp = NegateNum
          deriving (Eq, Show)

cmtUnOpTable = [("-", NegateNum)]

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
          | DeclareAuto String
          | Declare String Expr
          deriving (Eq, Show)

data FunDecl = FunDecl {
    name :: String,
    ret :: Type,
    args :: [(String, Type)],
    body :: Stmt
} deriving (Eq, Show)

type Prog = [FunDecl]

type NameEnv = M.Map VarName Type
