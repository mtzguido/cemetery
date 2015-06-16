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
    do d' <- tr_gdecl d
       return $ IR.Decl d'

translate1 (A.FunDecl {A.name = name, A.ret = ret,
                       A.args = args, A.body = body}) =
    do ir_ret <- tmap ret
       requestName name
       addToEnv name (envv { typ = fun_t args ret, ir_name = name })
       let tr_arg (s, t) = do s' <- requestSimilar s
                              t' <- tmap t
                              addToEnv s (envv { typ = t,
                                                 ir_name = s' })
                              return (s', t')

       pushLevel
       ir_args <- mapM tr_arg args

       setRetType ret
       ir_body_s <- tr_stmt body
       l <- popLevel

       let ft = IR.Funtype { IR.name = name,
                             IR.args = ir_args,
                             IR.ret = ir_ret }
       return $ IR.FunDef ft (reverse $ decls l, ir_body_s)

tr_body :: A.Stmt -> TM IR.Block
tr_body b = do pushLevel
               s <- tr_stmt b
               l <- popLevel
               return (reverse $ decls l, s)

tr_stmt :: A.Stmt -> TM IR.Stmt
tr_stmt A.Skip =
    do return IR.Skip

tr_stmt (A.Assign n e) =
    do d <- env_lookup n
       (p_e, t_e, ir_e) <- tr_expr e

       abortIf (elem RO (attrs d)) "Can't assign to const"
       abortIf (not (tmatch t_e (typ d))) "Type mismatch in assignment"

       return $ sseq p_e (IR.Assign (IR.LVar (ir_name d)) (ir_e))

tr_stmt (A.Return e) =
    do rt <- getRetType
       (p_e, t_e, ir_e) <- tr_expr e
       abortIf (not (tmatch rt t_e)) "Invalid return"
       return $ sseq p_e (IR.Return ir_e)

tr_stmt (A.Seq l r) =
    do ll <- tr_stmt l
       rr <- tr_stmt r
       return $ sseq ll rr

tr_stmt (A.Decl d) =
    do d' <- tr_ldecl d
       addDecl d'
       return IR.Skip

tr_stmt (A.If c t e) =
    do (prep, c_t, c_ir) <- tr_expr c
       abortIf (not (tmatch c_t A.Bool))
           "If conditions have to be of type Bool"
       tt <- tr_body t
       ee <- tr_body e
       return $ sseq prep (IR.If c_ir tt ee)


-- The bool represents wether we're translating
-- an initializer, so function calls are prohibited.
tr_expr = tr_expr' False
tr_init = tr_expr' True
tr_expr' :: Bool -> A.Expr -> TM (IR.Stmt, A.Type, IR.Expr)
tr_expr' i (A.ConstInt ii) =
    do return (IR.Skip, A.Int, IR.ConstInt ii)

tr_expr' i (A.ConstBool b) =
    do return (IR.Skip, A.Bool, IR.ConstBool b)

tr_expr' i (A.BinOp op l r) =
    do (l_p, l_t, l_ir) <- tr_expr' i l
       (r_p, r_t, r_ir) <- tr_expr' i r
       ops <- find_matching_binop op l_t r_t
       abortIf (ops == []) $ "Error on binary operator: " ++ show op
       let (e_t, ir_op) = head ops
       return (sseq l_p r_p, e_t, IR.BinOp ir_op l_ir r_ir)

tr_expr' i (A.UnOp op l) =
    do (l_p, l_t, l_ir) <- tr_expr' i l
       ops <- find_matching_unop op l_t
       abortIf (ops == []) $ "Error on unary operator: " ++ show op
       let (e_t, ir_op) = head ops
       return (l_p, e_t, IR.UnOp ir_op l_ir)

tr_expr' i (A.Var n) =
    do d <- env_lookup n
       return (IR.Skip, typ d, IR.LV (IR.LVar (ir_name d)))

tr_expr' i (A.Call f args) =
    do abortIf i "Can't call functions in initializers"
       d <- env_lookup f
       let A.Fun expected_t ret = typ d
       ir_ret <- tmap ret
       temp <- fresh ir_ret

       as <- mapM (tr_expr' i) args
       let (args_prep, actual_t, args_ir) = unzip3 as

       abortIf (length actual_t /= length expected_t)
           "Wrong number of arguments on function call"

       let ok = zipWith tmatch actual_t expected_t
       abortIf (not (all id ok))
           "Ill typed function argument on call"

       let prep = IR.Assign temp (IR.Call (ir_name d) args_ir ir_ret)

       -- We unconditionally assign the result of the call to a
       -- temporary variable and then return the variable. This allows
       -- to keep track of all objects which need to be freed. We could
       -- later only do this for buffers and such, since this tracking
       -- isn't necessary for ints,bools, etc.
       return (sseq (foldl sseq IR.Skip args_prep) prep,
               ret,
               IR.LV temp)

tr_expr' i (A.Arr es) =
    do (es_preps, es_types, es_irs) <- liftM unzip3 $ mapM (tr_expr' i) es
       abortIf (not $ all (== head es_types) es_types)
         "All elements of the array need to have the same type"

       return (foldl sseq IR.Skip es_preps,
               A.ArrT (head es_types),
               IR.Arr es_irs)

tr_expr' i (A.ConstFloat _) =
    do abort "Floats unsupported"

tr_expr' i (A.ConstStr _) =
    do abort "Strings unsupported"

tr_expr' i (A.BinLit _) =
    do abort "Binary literals unsupported"

tmap :: A.Type -> TM IR.Type
tmap A.Int =
    do return IR.Int

tmap A.Bool =
    do return IR.Bool

tmap (A.ArrT t) =
    do t' <- tmap t
       return (IR.ArrT t')

tmap t =
    do abort $ "Can't map that type (" ++ (show t) ++ ")"

-- Declaration translation

tr_gdecl (A.VarDecl n mods _      Nothing) =
    do abort "Global constants need an initializer"

-- TODO: Prevent function calls on global initializers
tr_gdecl (A.VarDecl n mods (Just t) (Just e)) =
    do (prep, e_t, e_ir) <- tr_init e
       abortIf (prep /= IR.Skip) "Internal error"
       abortIf (not (tmatch e_t t)) "Type and initializer don't match"
       tr_gdecl' n mods e_t e_ir

tr_gdecl (A.VarDecl n mods Nothing  (Just e)) =
    do (prep, e_t, e_ir) <- tr_init e
       abortIf (prep /= IR.Skip) "Internal error"
       tr_gdecl' n mods e_t e_ir

tr_gdecl' :: String -> [A.VarModifiers] -> A.Type -> IR.Expr -> TM IR.Decl
tr_gdecl' n mods typ ir =
    do requestName n
       abortIf (not (elem A.Const mods))
           "Global variables can only be constants"

       addToEnv n (envv { typ = typ, ir_name = n, attrs = [RO] })
       ir_t <- tmap typ

       -- At this point, we'll need to simplify ir
       -- to a "static" form, since it's allowed
       -- to initialize a variable with || (concatenation)
       -- (once that's done) but that will likely result in a
       -- function call. Either reduce everything or prepare
       -- to do so in a cmt_init().
       return $ IR.DeclareVar n ir_t ir

tr_ldecl (A.VarDecl n mods Nothing Nothing) =
    do abort "Variables need a type or an initializer"

tr_ldecl (A.VarDecl n mods (Just t) Nothing) =
    do abortIf (elem A.Const mods) "Constants need an initializer"
       tr_ldecl' n mods t (default_initializer t)

tr_ldecl (A.VarDecl n mods (Just t) (Just e)) =
    do (IR.Skip, e_t, e_ir) <- tr_expr e
       abortIf (not (tmatch e_t t)) "Type and initializer don't match"
       tr_ldecl' n mods e_t e_ir

tr_ldecl (A.VarDecl n mods Nothing  (Just e)) =
    do (IR.Skip, e_t, e_ir) <- tr_expr e
       tr_ldecl' n mods e_t e_ir

-- declared
tr_ldecl' :: String -> [A.VarModifiers] -> A.Type -> IR.Expr -> TM IR.Decl
tr_ldecl' n mods typ ir =
    do n' <- requestSimilar n

       let attrs = if elem A.Const mods
                   then [RO]
                   else []

       abortIf (elem A.Extern mods) "External on local scope?"

       addToEnv n (envv { typ = typ, ir_name = n', attrs = attrs })
       ir_t <- tmap typ
       return $ IR.DeclareVar n' ir_t ir
