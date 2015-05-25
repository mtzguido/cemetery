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

data Reg = Temp Int
  deriving (Eq)

instance Show Reg where
  show (Temp i) = "r" ++ show i

regn :: Int -> Reg
regn n = Temp n

data BinOp = Plus | Minus | Div | Prod
  deriving (Eq)

instance Show BinOp where
  show Plus = "+"
  show Minus = "-"
  show Div = "/"
  show Prod = "*"

data Stmt = AssignInt   Reg Int
          | Assign      Reg Reg
          | AssignOp    BinOp Reg Reg Reg   -- First one is result
          | Return      Reg
          | Seq         Stmt Stmt
          | StmtScaf
          | Skip                            -- Simply discard this
          | If          Reg Stmt Stmt       -- C-like semantics
  deriving (Eq)

indent' [] = []
indent' "\n" = "\n"
indent' (c:cs) = if c == '\n'
                   then c : indent cs
                   else c : indent' cs
indent s = ' ' : indent' s

instance Show Stmt where
  show (AssignInt r i) = show r ++ " <- #" ++ show i ++ "\n"
  show (Assign r s) = show r ++ " <- " ++ show s ++ "\n"
  show (AssignOp op r s t) = show r ++ " <- " ++ show s ++ " " ++
                             show op ++ " " ++ show t ++ "\n"
  show (Return r) = "return: " ++ show r ++ "\n"
  show (Seq l r) = show l ++ show r
  show (Skip) = "\n"
  show (StmtScaf) = "I.O.U.\n"
  show (If r t e) = "IF (" ++ show r ++ ")\n" ++ indent (show t)
                     ++ "ELSE\n" ++ indent (show e)
