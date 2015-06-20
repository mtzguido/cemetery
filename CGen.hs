module CGen where

import Data.List
import Data.Monoid
import qualified IR as I
import qualified CLang as C
import Common

import Control.Monad.Writer
import Control.Monad.Identity

bitsType = C.Custom "cmt_bits_t"

cgen :: I.IR -> C.Prog
cgen ir = let (units, ()) = runGM (g_ir ir)
           in C.Prog { C.includes = ["stdbool"], C.units = units}

type GM = WriterT [C.Unit] (
           Identity
          )

sseq C.Skip r = r
sseq l C.Skip = l
sseq l r = C.Seq l r

runGM :: GM t -> ([C.Unit], t)
runGM m = let m' = runWriterT m
              (r, units) = runIdentity m'
           in (units, r)

g_ir :: I.IR -> GM ()
g_ir p = do bs <- mapM g_unit p
            return ()

g_unit :: I.Unit -> GM ()
g_unit (I.Decl d) =
    do d' <- g_decl d
       tell [C.Decl d']

g_unit (I.FunDef (I.Funtype { I.name = name,
                              I.args = args,
                              I.ret  = ret}) body) =
    do c_args <- mapM g_arg args
       c_ret <- g_type ret
       c_body <- g_body body
       let ft = C.Funtype { C.name = name, C.args = c_args, C.ret = c_ret }
       tell [C.FunDef ft c_body]

g_arg :: (String, I.Type) -> GM (String, C.Type)
g_arg (n, t) =
    do t' <- g_type t
       return (n, t')

g_body :: I.Block -> GM C.Block
g_body (d, s) =
    do d' <- mapM g_decl d
       s' <- g_stmt s
       return (d', s')

g_decl :: I.Decl -> GM C.Decl
g_decl (I.DeclareTemp i t) =
    do let v = "cmt_temp_" ++ show i
       tt <- g_type t
       return $ C.VarDecl v tt Nothing []

g_decl (I.DeclareGlobal n t e) =
    do tt <- g_type t
       e_c <- g_expr e
       return $ C.VarDecl n tt (Just e_c) []

g_decl (I.DeclareVar n t) =
    do tt <- g_type t
       return $ C.VarDecl n tt Nothing []

g_stmt :: I.Stmt -> GM C.Stmt
g_stmt (I.Seq l r) =
    do ll <- g_stmt l
       rr <- g_stmt r
       return $ sseq ll rr

g_stmt I.Skip =
    do return C.Skip

g_stmt (I.Assign lv e) =
    do c_e <- g_expr e
       c_lv <- g_lvalue lv
       return (C.Expr $ C.Assign c_lv c_e)

g_stmt (I.If c t e) =
    do c_c <- g_expr c
       c_t <- g_body t
       c_e <- g_body e
       return (C.If c_c c_t c_e)

g_stmt (I.Return e) =
    do c_e <- g_expr e
       return (C.Return c_e)

g_stmt (I.For lv fr to b) =
    do i <- g_lvalue lv
       f <- g_expr fr
       t <- g_expr to
       let init = C.Assign i f
       let cond = C.BinOp C.Le (C.LV i) t
       let inc = C.Assign i (C.BinOp C.Plus (C.LV i) (C.ConstInt 1))
       body <- g_body b
       return $ C.For init cond inc body

g_expr :: I.Expr -> GM C.Expr
g_expr (I.ConstInt i) =
    do return $ C.ConstInt i

g_expr (I.ConstBool b) =
    do return $ C.ConstBool b

g_expr (I.BinOp op l r) =
    do ll <- g_expr l
       rr <- g_expr r
       g_binop op ll rr

g_expr (I.UnOp op l) =
    do ll <- g_expr l
       g_unop op ll

g_expr (I.LV lv) =
    do lv_c <- g_lvalue lv
       return $ C.LV lv_c

g_expr (I.Call n args) =
    do c_args <- mapM g_expr args
       return $ C.Call n c_args

g_expr (I.Arr es) =
    do es' <- mapM g_expr es
       return $ C.Arr es'

g_expr (I.Access a i) =
    do aa <- g_expr a
       ii <- g_expr i
       return $ C.Access aa ii

g_expr (I.Slice a f t) =
    do aa <- g_expr a
       ff <- g_expr f
       tt <- g_expr t
       return $ C.Call "__cmt_slice" [aa, ff, tt]

g_type I.Int  = do return C.Int
g_type I.Bool = do return C.Bool
g_type I.Bits = do return bitsType
g_type (I.ArrT t) =
    do t' <- g_type t
       return (C.ArrT t')

g_binop I.Plus  l r = do return $ C.BinOp C.Plus  l r
g_binop I.Minus l r = do return $ C.BinOp C.Minus l r
g_binop I.Div   l r = do return $ C.BinOp C.Div   l r
g_binop I.Prod  l r = do return $ C.BinOp C.Prod  l r
g_binop I.Eq    l r = do return $ C.BinOp C.Eq    l r
g_binop I.Mod   l r = do return $ C.BinOp C.Mod   l r
g_binop I.And   l r = do return $ C.BinOp C.And   l r
g_binop I.Or    l r = do return $ C.BinOp C.Or    l r
g_binop I.Lt    l r = do return $ C.BinOp C.Lt    l r
g_binop I.Le    l r = do return $ C.BinOp C.Le    l r
g_binop I.Gt    l r = do return $ C.BinOp C.Gt    l r
g_binop I.Ge    l r = do return $ C.BinOp C.Ge    l r
g_binop I.Band  l r = do return $ C.Call "__cmt_band" [l, r]
g_binop I.Bor   l r = do return $ C.Call "__cmt_bor" [l, r]
g_binop I.Xor   l r = do return $ C.Call "__cmt_xor" [l, r]
g_binop I.BConcat l r = do return $ C.Call "__cmt_bconcat" [l, r]
g_binop I.LShift l r = do return $ C.Call "__cmt_shiftl" [l, r]
g_binop I.RShift l r = do return $ C.Call "__cmt_shiftr" [l, r]
g_binop I.LRot l r  = do return $ C.Call "__cmt_rotl" [l, r]
g_binop I.RRot l r  = do return $ C.Call "__cmt_rotr" [l, r]

g_unop  I.Neg   e   = do return $ C.UnOp C.NegateNum e
g_unop  I.Not   e   = do return $ C.UnOp C.Not       e
g_unop  I.Bnot  e   = do return $ C.Call "__cmt_bnot" [e]

g_lvalue (I.LVar n) =
    do return $ C.LVar n

g_lvalue (I.Temp i) =
    do return $ C.LVar ("cmt_temp_" ++ show i)
