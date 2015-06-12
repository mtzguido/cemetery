module CGen where

import Data.List
import Data.Monoid
import qualified IR as I
import qualified CLang as C
import Common

import Control.Monad.Writer
import Control.Monad.Identity

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

g_decl (I.DeclareVar n t me) =
    do tt <- g_type t
       me_c <- fmapM_maybe g_expr me
       return $ C.VarDecl n tt me_c []

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
       oo <- g_binop op
       return $ C.BinOp oo ll rr

g_expr (I.UnOp op l) =
    do ll <- g_expr l
       oo <- g_unop op
       return $ C.UnOp oo ll

g_expr (I.LV (I.LVar n)) =
    do return $ C.Var n

g_expr (I.LV (I.Temp i)) =
    do return $ C.Var ("cmt_temp_" ++ show i)

g_expr (I.Call n args _) =
    do c_args <- mapM g_expr args
       return $ C.Call n c_args

g_type I.Int  = do return C.Int
g_type I.Bool = do return C.Bool

g_binop I.Plus  = do return C.Plus
g_binop I.Minus = do return C.Minus
g_binop I.Div   = do return C.Div
g_binop I.Prod  = do return C.Prod
g_binop I.Eq    = do return C.Eq
g_binop I.Mod   = do return C.Mod
g_binop I.And   = do return C.And
g_binop I.Or    = do return C.Or

g_unop I.Neg = do return C.NegateNum
g_unop I.Not = do return C.Not
