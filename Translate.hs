-- Cemetery translation module
--
-- This module takes care of translating the Cemetery AST just after
-- parsing into an intermediate representation for it to be later
-- translated into the output language.
--
-- Afterwards, this should go into the Output module (unexistant as of
-- now) which should generate the end result (C Code)
--
-- Right now, this basically fill everything with stubs.

module Translate where

import qualified AST as A
import qualified IR as IR
import Builtins
import Type
import TMonad

import Common
import Control.Monad
import Data.Maybe

semanticT :: A.Prog -> (TransState, Either CmtError IR.IR)
semanticT = runTranslate.translate

-- IR Helpers

sseq :: IR.Stmt -> IR.Stmt -> IR.Stmt
sseq (IR.Skip) r = r
sseq l (IR.Skip) = l
sseq l r = IR.Seq l r

irlist :: [IR.Stmt] -> IR.Stmt
irlist l = foldl sseq IR.Skip l

add_builtins =
    do mapM (uncurry addToEnv) builtins

-- These functions take care of name clashes,
-- the first one fails, while the second one
-- tries to find a similar unused name (TODO)

requestName s =
    do return ()
requestSimilar s =
    do return s

-- Each Cemetery unit is a IR unit, at least for now,
-- so just mapM the unit translation
translate :: A.Prog -> TM IR.IR
translate prog = do -- Push a first level and add builtins to it
                    setData [blank_level]
                    add_builtins
                    ir <- mapM translate1 prog
                    last_lvl <- popLevel
                    return ir

fun_t args ret = A.Fun (map snd args) ret

translate1 :: A.Decl -> TM IR.Unit
translate1 d@(A.VarDecl _ _ _ _) =
    do d' <- tr_decl d
       return $ IR.Decl d'

translate1 (A.Struct) =
    do error "structs are not supported"

translate1 (A.FunDecl {A.name = name, A.ret = ret,
                       A.args = args, A.body = body}) =
    do ir_ret <- tmap ret
       requestName name
       addToEnv name (EnvV { typ = fun_t args ret, ir_name = name })
       let tr_arg (s, t) = do s' <- requestSimilar s
                              t' <- tmap t
                              addToEnv s (EnvV { typ = t,
                                                 ir_name = s' })
                              return (s', t')

       ir_args <- mapM tr_arg args

       pushLevel
       setRetType ret
       ir_body_s <- tr_stmt body
       l <- popLevel

       let ft = IR.Funtype { IR.name = name,
                             IR.args = ir_args,
                             IR.ret = ir_ret }
       return $ IR.FunDef ft (reverse $ decls l, ir_body_s)

tr_body :: A.Stmt -> TM IR.Block
tr_body b = do s <- tr_stmt b
               return ([], s)

tr_stmt :: A.Stmt -> TM IR.Stmt
tr_stmt A.Skip =
    do return IR.Skip
tr_stmt (A.Assign n e) =
    do d <- env_lookup n
       (t_e, ir_e) <- tr_expr e
       when (not (tmatch t_e (typ d))) (error "type mismatch")
       return $ IR.Assign (IR.LVar (ir_name d)) (ir_e)

tr_stmt (A.Return e) =
    do rt <- getRetType
       (t_e, ir_e) <- tr_expr e
       when (not (tmatch rt t_e)) (error "invalid return")
       return (IR.Return ir_e)

tr_stmt (A.Seq l r) =
    do ll <- tr_stmt l
       rr <- tr_stmt r
       return $ sseq ll rr

tr_stmt (A.Decl d) =
    do d' <- tr_decl d
       addDecl d'
       return IR.Skip

tr_stmt (A.If c t e) =
    do (c_t, c_ir) <- tr_expr c
       when (not (tmatch c_t A.Bool))
        (error "If conditions have to be of type Bool")
       tt <- tr_body t
       ee <- tr_body e
       return $ IR.If c_ir tt ee

tr_decl (A.VarDecl n mods Nothing  Nothing) =
    do error "Variables need either a type or an initializer"

tr_decl (A.VarDecl n mods (Just t) Nothing) =
    do tr_vdecl n mods t (Just $ default_initializer t)

-- TODO: Prevent function calls on global initializers
tr_decl (A.VarDecl n mods (Just t) (Just e)) =
    do (e_t, e_ir) <- tr_expr e
       when (not (tmatch e_t t))
         (error "Type an initializer don't match")
       tr_vdecl n mods e_t (Just e_ir)

tr_decl (A.VarDecl n mods Nothing  (Just e)) =
    do (e_t, e_ir) <- tr_expr e
       tr_vdecl n mods e_t (Just e_ir)

tr_vdecl :: String -> [A.VarModifiers] -> A.Type -> Maybe IR.Expr -> TM IR.Decl
tr_vdecl n mods typ ir =
    do n' <- requestSimilar n   -- TODO: use requestName when global decl
       addToEnv n (EnvV { typ = typ, ir_name = n' })
       ir_t <- tmap typ
       return $ IR.DeclareVar n' ir_t ir

tr_expr :: A.Expr -> TM (A.Type, IR.Expr)
tr_expr (A.ConstInt i) =
    do return (A.Int, IR.ConstInt i)

tr_expr (A.ConstBool b) =
    do return (A.Bool, IR.ConstBool b)

tr_expr (A.BinOp op l r) =
    do (l_t, l_ir) <- tr_expr l
       (r_t, r_ir) <- tr_expr r
       (e_t, ir_op) <- liftM head (find_matching_binop op l_t r_t)
       return (e_t, IR.BinOp ir_op l_ir r_ir)

tr_expr (A.UnOp op l) =
    do (l_t, l_ir) <- tr_expr l
       (e_t, ir_op) <- liftM head (find_matching_unop op l_t)
       return (e_t, IR.UnOp ir_op l_ir)

tr_expr (A.Var n) =
    do d <- env_lookup n
       return (typ d, IR.Var (ir_name d))

tr_expr (A.Call f args) =
    do d <- env_lookup f
       let A.Fun expected_t ret = typ d
       as <- mapM tr_expr args
       let (actual_t, args_ir) = unzip as

       let ok = zipWith tmatch actual_t expected_t
       when (not (all id ok)) (error "ill typed function argument")

       return (ret, IR.Call (ir_name d) args_ir)

tr_expr (A.ConstFloat _) =
    do error "Floats unsupported"

tr_expr (A.ConstStr _) =
    do error "Strings unsupported"

tr_expr (A.BinLit _) =
    do error "Binary literals unsupported"

tmap :: A.Type -> TM IR.Type
tmap A.Int  = do return IR.Int
tmap A.Bool = do return IR.Bool
tmap t = error $ "Can't map that type (" ++ (show t) ++ ")"
