-- Cemetery optimization module
--
-- This will, in the long run, include rules to optimize the
-- intermediate representation so we can generate a better source code
-- without depending on the output language.
--
-- We could later implement language-specific optimization if needed.
--
-- Do nothing for now.

module Optimize where

import Control.Monad
import Control.Monad.Identity

import IR

type OM = Identity

optimize :: IR -> IR
optimize p = runIdentity $ mapM o_unit p

o_unit :: Unit -> OM Unit
o_unit (FunDef ft b) =
    do b' <- o_body b
       return $ FunDef ft b'
o_unit x =
    do return x

o_body :: Block -> OM Block
o_body (d, s) =
    do s' <- o_stmt s
       return $ (d, s')

o_stmt :: Stmt -> OM Stmt
o_stmt (Assign l e) =
    do e' <- o_expr e
       return $ Assign l e'

o_stmt (Return e) =
    do e' <- o_expr e
       return $ Return e'

o_stmt x =
    do return x

o_expr :: Expr -> OM Expr
o_expr (BinOp Plus (ConstInt l) (ConstInt r)) =
    do return $ ConstInt (l + r)

o_expr e =
    do return e
