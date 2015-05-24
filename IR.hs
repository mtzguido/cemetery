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
          | UnitScaf
  deriving (Eq, Show)

data Funtype = Funtype { name :: String,
                         args :: [(String, Type)],
                         ret :: Type }
  deriving (Eq, Show)

data Reg = Temp Int | Var String
  deriving (Eq, Show)

regn :: Int -> Reg
regn n = Temp n

data BinOp = Plus | Minus | Div | Prod
  deriving (Eq, Show)

data Stmt = AssignInt   Reg Int
          | Assign      Reg Reg
          | AssignOp    BinOp Reg Reg Reg
          | Return      Reg
          | Seq         Stmt Stmt
          | StmtScaf
          | Skip                            -- Simply discard this
  deriving (Eq, Show)
