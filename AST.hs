module AST where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.Maybe

type VarName = String

data Type = Int
          | Bool
          | String
          | Bits
          | Void
          | Double
          | Fun [Type] Type
          | ArrT Type (Maybe Int)
          | Invalid -- Dummy type
          deriving (Eq, Show)

cmtTypeTable = [("int", Int), ("bool", Bool), ("string", String),
                ("bits", Bits), ("void", Void), ("double", Double)]

data BinOp = Plus | Minus | Div | Prod | Eq | Mod
           | Xor | And | Or | Band | Bor | BConcat
           | LShift | RShift | LRot | RRot
           | Le | Ge | Lt | Gt
          deriving (Eq, Show)

data UnOp = Neg | Not | Bnot
          deriving (Eq, Show)

data Expr = ConstInt Int
          | ConstFloat Double
          | ConstBool Bool
          | BinOp BinOp Expr Expr
          | UnOp UnOp Expr
          | Call VarName [Expr]
          | Var VarName
          | ConstStr String
          | BinLit [Int] Int  -- bytes and length in *bits*
          | Arr [Expr]
          | Slice Expr Expr Expr -- Array, From, To
          | Access Expr Expr -- Array, Pos
          deriving (Eq, Show)

data Stmt = Skip
          | Assign VarName Expr
          | If Expr Stmt Stmt
          | Seq Stmt Stmt
          | Return Expr
          | Decl Decl
          | For VarName Expr Expr Stmt
          | Err String
          deriving (Eq, Show)

data Mods = Const | Extern | Static
    deriving (Eq, Show)

data Decl = VarDecl String [Mods] (Maybe Type) (Maybe Expr)
          | FunDecl { name :: String,
                      ret :: Type,
                      args :: [(String, Type)],
                      mods :: [Mods],
                      body :: Stmt }
          deriving (Eq, Show)

type Prog = [Decl]

sseq Skip s = s
sseq s Skip = s
sseq s t = Seq s t
