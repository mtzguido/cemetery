-- Cemetery intermediate representation.
--
-- It's similar to what you would have in a regular machine-code
-- compiler, except that we don't need to completely flatten it since
-- we're compiling into a high-level language which supports branching
-- and looping.
--
-- For now, this IR only represents functions that deal with integers.

module IR where

import Data.Word

type IR = [Unit]

data Type = Int
  deriving (Eq, Show)

data Unit = FunDef Funtype Stmt
          | Scaf
  deriving (Eq, Show)

data Funtype = Funtype { name :: String,
                         args :: [(String, Type)],
                         ret :: Type }
  deriving (Eq, Show)

data Reg = Reg Int
  deriving (Eq, Show)

regn :: Int -> Reg
regn n = Reg n

data BinOp = Plus | Minus | Div | Prod
  deriving (Eq, Show)

data Stmt = AssignInt   Reg Int
          | AssignOp    BinOp Reg Reg Reg
          | Return      Reg
          | Seq         Stmt Stmt
  deriving (Eq, Show)
