module Type where

import Common
import TMonad
import AST
import qualified IR

tmatch :: Type -> Type -> Bool
tmatch (ArrT a _) (ArrT b _) = tmatch a b
tmatch p q = p == q

default_initializer :: Type -> TM IR.Expr
default_initializer Int  =
    do return $ IR.ConstInt 0

default_initializer Bool =
    do return $ IR.ConstBool False

default_initializer Bits =
    do return $ IR.ConstBits [] 0

default_initializer (ArrT t Nothing) =
    do abort "no length and no init"

default_initializer (ArrT t (Just l)) =
    do e' <- default_initializer t
       return $ IR.Arr (replicate l e')
