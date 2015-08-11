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
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Common
import IR
import Liveness

data OMState =
  OMState {
    -- Counter to get fresh temp variables
    counter :: Int
  }

init_state = OMState { counter = 0 }

type OM = ErrorT CmtError (
           StateT OMState (
            Identity
          ))

runOM m = runIdentity $ runStateT (runErrorT m) init_state

optimize :: IR -> OM IR
optimize p = mapM o_unit p

o_unit (FunDef ft b) =
    do let (_, s) = b
       c <- check_return_paths s
       when (not c) $ throwError $
           CmtErr ("Some paths in '" ++ name ft ++ "' do not return!")

       b <- o_body b

       let b' = liveness b
       return $ FunDef ft b'

o_unit (Decl d) = -- do nothing with declarations
    do return $ Decl d

o_body (bd, bs) =
    do bs <- expr_opt const_fold bs
       return (bd, bs)

---------------------------------------------------------------------
-- Return path checking
---------------------------------------------------------------------

check_return_paths (Return _) =
    do return True

check_return_paths (Error _) =
    do return True

check_return_paths (Seq l r) =
    do lp <- check_return_paths l
       rp <- check_return_paths r
       return $ lp || rp

check_return_paths (If _ (_,t) (_,e)) =
    do tp <- check_return_paths t
       ep <- check_return_paths e
       return $ tp && ep

check_return_paths (For _ _ _ (_,b)) =
    do check_return_paths b

check_return_paths _ =
    do return False

---------------------------------------------------------------------
-- Generic code for traversing statements until expressions
---------------------------------------------------------------------

expr_opt f (Return e) =
    do e' <- f e
       return $ Return e'

expr_opt f (Error s) =
    do return $ Error s

expr_opt f (Seq l r) =
    do l' <- expr_opt f l
       r' <- expr_opt f r
       return $ Seq l' r'

expr_opt f (If c (td, ts) (ed, es)) =
    do c' <- f c
       ts' <- expr_opt f ts
       es' <- expr_opt f es
       return $ If c' (td, ts') (ed, es')

expr_opt f (For i l h (bd, bs)) =
    do l' <- f l
       h' <- f h
       bs' <- expr_opt f bs
       return $ For i l' h' (bd, bs')

expr_opt f Skip =
    do return Skip

expr_opt f (Assign v e) =
    do e' <- f e
       return $ Assign v e'

expr_opt f (Free _) =
    do error "Free before liveness analysis?"

---------------------------------------------------------------------
-- Constant folding
---------------------------------------------------------------------
const_fold e@(ConstInt _)     = do return e
const_fold e@(ConstBool _)    = do return e
const_fold e@(ConstBits _ _ ) = do return e
const_fold e@(Copy _)         = do return e
const_fold e@(LV _)           = do return e
const_fold e@(Arr _)          = do error "local array on constant folding"

const_fold (Slice b f t) =
    do f' <- const_fold f
       t' <- const_fold t
       return $ Slice b f' t'

const_fold (Access a i) =
    do i' <- const_fold i
       return $ Access a i'

const_fold (Call n args) =
    do args' <- mapM const_fold args
       return $ Call n args'

const_fold (BinOp op l r) =
    do l' <- const_fold l
       r' <- const_fold r
       return $ fold_binop op l' r'

const_fold (UnOp op e) =
    do e' <- const_fold e
       return $ fold_unop op e'

const_fold (Cluster op es) =
    do return $ Cluster op es

fold_binop Plus  (ConstInt x) (ConstInt y) = ConstInt (x + y)
fold_binop Minus (ConstInt x) (ConstInt y) = ConstInt (x - y)
fold_binop Div   (ConstInt x) (ConstInt y) = ConstInt (div x y)
fold_binop Prod  (ConstInt x) (ConstInt y) = ConstInt (x * y)
fold_binop Mod   (ConstInt x) (ConstInt y) = ConstInt (mod x y) --FIXME: y<0?

fold_binop And (ConstBool p) (ConstBool q) = ConstBool (p && q)
fold_binop Or  (ConstBool p) (ConstBool q) = ConstBool (p || q)

fold_binop op l r = BinOp op l r

fold_unop Neg (ConstInt x) = ConstInt (-x)
fold_unop Not (ConstBool b) = ConstBool (not b)

fold_unop op e = UnOp op e
