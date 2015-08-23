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
import Data.Maybe (fromJust)

type IR = [Unit]

data Type = Int | Bool | ArrT Type (Maybe Int)
          | Bits
  deriving (Eq, Show, Ord)

data Unit = FunDef Funtype Block
          | Decl Decl
  deriving (Eq, Show)

data Funtype = Funtype { name :: String,
                         args :: [(String, Type)],
                         mods :: [Mods],
                         ret :: Type }
  deriving (Eq, Show)

data BinOp = Plus | Minus | Div | Prod | Eq | Mod | And | Or
           | Band | Bor | BConcat | Xor
           | LShift | RShift | LRot | RRot
           | Le | Lt | Gt | Ge
           | ModPlus -- Bits addition modulo size
           | BitEq
  deriving (Eq, Show, Ord)

data Mods = Static
  deriving (Eq, Show)

data UnOp = Neg | Not | Bnot
  deriving (Eq, Show, Ord)

data LValue = LVar String
            | Temp Int
            | Builtin Builtin
            | Access LValue Expr
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
          | Slice      LValue Expr Expr
          | ConstBits  [Int] Int
          | Copy       LValue
            -- The bool represents if the value should be freed
            -- this always comes as False from translation, and
            -- the Liveness analysis might add True's
          | Cluster    ClusterExpr [(LValue, Bool)]
  deriving (Eq, Show, Ord)

data ClusterExpr = CBinOp BinOp ClusterExpr ClusterExpr
                 | CUnOp  UnOp  ClusterExpr
                 | CArg   Int
    deriving (Eq, Show, Ord)

type Block = ([Decl], Stmt)

data Decl = DeclLocal  LValue Type
          | DeclGlobal String Type Expr
  deriving (Eq, Show)

data Stmt = Assign      LValue Expr
          | Return      Expr
          | Seq         Stmt Stmt
          | Skip
          | If          Expr Block Block -- Expr is true if non-zero
          | For         LValue Expr Expr Block
          | Error       String
          | Free        [LValue]
          | FreeArr     [LValue]
  deriving (Eq, Show)

sseq Skip r = r
sseq l Skip = l
sseq l r = Seq l r

sfold = foldl sseq Skip

c_binop op (Cluster le la) (Cluster re ra) =
    let ea = la ++ ra
        ee = CBinOp op le (argmap (\i -> i + length la) re)
     in c_simplify $ Cluster ee ea

c_unop op (Cluster le la) =
    let ea = la
        ee = CUnOp op le
     in c_simplify $ Cluster ee ea

c_simplify (Cluster e a) =
    let mapping = map (\i -> fromJust $ elemIndex i (nub a)) a
        e' = argmap (mapping!!) e
     in Cluster e' (nub a)

argmap f (CBinOp op l r) = CBinOp op (argmap f l) (argmap f r)
argmap f (CUnOp op l)    = CUnOp op (argmap f l)
argmap f (CArg i)        = CArg (f i)
