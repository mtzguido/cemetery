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

import IR

optimize :: IR -> IR
optimize p = map o_unit p

o_unit UnitScaf = UnitScaf
o_unit (FunDef ft body) = FunDef ft (o_stmt body)

o_stmt (Assign l e) = Assign l (o_expr e)
o_stmt (Return e) = Return (o_expr e)
o_stmt x = x

o_expr (BinOp Plus (ConstInt l) (ConstInt r)) = ConstInt (l + r)
o_expr e = e
