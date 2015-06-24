module Type where

import Common
import TMonad
import AST
import qualified IR

tmatch :: Type -> Type -> Bool
tmatch p q = p == q

default_initializer :: Type -> IR.Expr
default_initializer Int  = IR.ConstInt 0
default_initializer Bool = IR.ConstBool False
default_initializer Bits = IR.ConstBits [] 0
