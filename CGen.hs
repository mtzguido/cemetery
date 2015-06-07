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
           in C.Prog { C.includes = [], C.units = units}

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
g_unit (I.FunDef (I.Funtype { I.name = name,
                              I.args = args,
                              I.ret  = ret}) body) =
    do c_args <- g_args args
       c_ret <- g_type ret
       c_body <- g_body body
       let ft = C.Funtype { C.name = name, C.args = c_args, C.ret = c_ret }
       tell [C.FunDef ft c_body]

g_unit (I.UnitScaf) =
    do tell [C.Comment "UnitScaf"]

g_args :: [(String, I.Type)] -> GM [(String, C.Type)]
g_args _ = do return []

g_body :: I.Stmt -> GM C.Block
g_body b = do s <- g_stmt b
              return ([], s)

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

g_stmt (I.If c t e) =
    do c_c <- g_expr c
       c_t <- g_body t
       c_e <- g_body e
       return (C.If c_c c_t c_e)

g_stmt (I.Return e) =
    do c_e <- g_expr e
       return (C.Return c_e)

g_expr :: I.Expr -> GM C.Expr
g_expr _ =
    do return $ C.ConstInt 2

g_type _ = do return C.Int
