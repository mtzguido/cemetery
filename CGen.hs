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

g_stmt (I.Assign (I.LVar s) e) =
    do c_e <- g_expr e
       return (C.Assign s c_e)

g_stmt (I.Assign (I.Temp i) e) =
    do c_e <- g_expr e
       let v = "cmt_temp_" ++ show i
       return (C.Assign v c_e)

g_stmt (I.If c t e) =
    do c_c <- g_expr c
       c_t <- g_body t
       c_e <- g_body e
       return (C.If c_c c_t c_e)

g_stmt (I.Return e) =
    do c_e <- g_expr e
       return (C.Return c_e)

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

g_expr (I.LV (I.LVar n)) =
    do return $ C.Var n

g_expr (I.LV (I.Temp i)) =
    do return $ C.Var ("cmt_temp_" ++ show i)

g_expr (I.Call n args) =
    do c_args <- mapM g_expr args
       return $ C.Call n c_args

g_expr (I.Arr es) =
    do es' <- mapM g_expr es
       return $ C.Arr es'

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
g_binop I.Band  l r = do return $ C.Call "__cmt_band" [l, r]
g_binop I.Bor   l r = do return $ C.Call "__cmt_bor" [l, r]
g_binop I.BConcat l r = do return $ C.Call "__cmt_bconcat" [l, r]

g_unop  I.Neg   e   = do return $ C.UnOp C.NegateNum e
g_unop  I.Not   e   = do return $ C.UnOp C.Not       e
g_unop  I.Bnot  e   = do return $ C.Call "__cmt_bnot" [e]
