module Type where

import Common
import TMonad
import AST
import qualified IR

tmatch :: Type -> Type -> Bool
tmatch p q = p == q

-- This structure describes how to map binary operators. For a given
-- Cemetery operator and the type of each of its two operands, we give
-- the type of the result and the corresponding IR operator.
--
-- In a future, we may use not only IR operators but function calls, or
-- even enable operator polymorphism.

binop_mapping = [
{-
 cmt_op   l_type r_type res_type ir_op
-}
 (Plus,   Int,   Int,   Int,     IR.Plus),
 (Minus,  Int,   Int,   Int,     IR.Minus),
 (Div,    Int,   Int,   Int,     IR.Div),
 (Prod,   Int,   Int,   Int,     IR.Prod),
 (Mod,    Int,   Int,   Int,     IR.Mod),
 (Eq,     Int,   Int,   Bool,    IR.Eq),
 (And,    Bool,  Bool,  Bool,    IR.And),
 (Or,     Bool,  Bool,  Bool,    IR.Or)
 ]

find_matching_binop :: BinOp -> Type -> Type -> TM [(Type, IR.BinOp)]
find_matching_binop op l_typ r_typ =
    do let l = filter (\(o, lt, rt, et, ir_op) ->
                op == o && tmatch l_typ lt && tmatch r_typ rt) binop_mapping
       return $ map (\(a,b,c,d,e) -> (d,e)) l

unop_mapping = [
{-
 cmt_op   e_type  res_type  ir_op
-}
 (Neg,    Int,    Int,      IR.Neg),
 (Not,    Bool,   Bool,     IR.Not)
 ]

find_matching_unop :: UnOp -> Type -> TM [(Type, IR.UnOp)]
find_matching_unop op e_typ =
    do let l = filter (\(o, et, rt, ir_op) ->
                op == o && tmatch e_typ et) unop_mapping
       return $ map (\(a,b,c,d) -> (c,d)) l

default_initializer :: Type -> IR.Expr
default_initializer Int  = IR.ConstInt 0
default_initializer Bool = IR.ConstBool False
