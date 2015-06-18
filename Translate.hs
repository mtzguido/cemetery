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
    do (p, d') <- tr_ldecl d
       addDecl d'
       return p

tr_stmt (A.If c t e) =
    do (prep, c_t, c_ir) <- tr_expr c
       abortIf (not (tmatch c_t A.Bool))
           "If conditions have to be of type Bool"
       tt <- tr_body t
       ee <- tr_body e
       return $ sseq prep (IR.If c_ir tt ee)


-- The bool represents wether we're translating a global initializer, so
-- function calls are prohibited.
tr_expr = tr_expr' False
tr_init = tr_expr' True
tr_expr' :: Bool -> A.Expr -> TM (IR.Stmt, A.Type, IR.Expr)
tr_expr' i (A.ConstInt ii) =
    do return (IR.Skip, A.Int, IR.ConstInt ii)

tr_expr' i (A.ConstBool b) =
    do return (IR.Skip, A.Bool, IR.ConstBool b)

tr_expr' i (A.BinOp op l r) =
    do tr_save $ tr_binop i op l r

tr_expr' i (A.UnOp op e) =
    do tr_save $ tr_unop i op e

tr_expr' i (A.Var n) =
    do d <- env_lookup n
       return (IR.Skip, typ d, IR.LV (IR.LVar (ir_name d)))

tr_expr' i (A.Call f args) =
    do tr_save $ tr_call i f args

tr_expr' i (A.Arr es) =
    do (es_preps, es_types, es_irs) <- liftM unzip3 $ mapM (tr_expr' i) es
       abortIf (not $ all (== head es_types) es_types)
         "All elements of the array need to have the same type"

       return (foldl sseq IR.Skip es_preps,
               A.ArrT (head es_types),
               IR.Arr es_irs)

tr_expr' i (A.Slice a f t) =
    tr_save (tr_slice i a f t)

tr_expr' i (A.Access a idx) =
    do (a_p, a_t, a_ir) <- tr_expr' i a
       (i_p, i_t, i_ir) <- tr_expr' i idx
       t <- case a_t of
                A.ArrT e -> return e
                _ -> abort "Accesses can only be used on arrays"

       abortIf (not $ tmatch i_t A.Int)
           "Access index has to be of type int"

       return (sseq a_p i_p,
               t,
               IR.Access a_ir i_ir)

tr_expr' i (A.ConstFloat _) =
    do abort "Floats unsupported"

tr_expr' i (A.ConstStr _) =
    do abort "Strings unsupported"

tr_expr' i (A.BinLit _) =
    do abort "Binary literals unsupported"

tmap :: A.Type -> TM IR.Type
tmap A.Int  = do return IR.Int
tmap A.Bool = do return IR.Bool
tmap A.Bits = do return IR.Bits

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
       return $ IR.DeclareGlobal n ir_t ir

tr_ldecl (A.VarDecl n mods Nothing Nothing) =
    do abort "Variables need a type or an initializer"

tr_ldecl (A.VarDecl n mods (Just t) Nothing) =
    do abortIf (elem A.Const mods) "Constants need an initializer"
       tr_ldecl' n mods IR.Skip t (default_initializer t)

tr_ldecl (A.VarDecl n mods (Just t) (Just e)) =
    do (p, e_t, e_ir) <- tr_expr e
       abortIf (not (tmatch e_t t)) "Type and initializer don't match"
       tr_ldecl' n mods p e_t e_ir

tr_ldecl (A.VarDecl n mods Nothing  (Just e)) =
    do (p, e_t, e_ir) <- tr_expr e
       tr_ldecl' n mods p e_t e_ir

tr_ldecl' :: String -> [A.VarModifiers] -> IR.Stmt -> A.Type -> IR.Expr
          -> TM (IR.Stmt, IR.Decl)
tr_ldecl' n mods p typ ir =
    do n' <- requestSimilar n

       let attrs = if elem A.Const mods
                   then [RO]
                   else []

       abortIf (elem A.Extern mods) "External on local scope?"

       addToEnv n (envv { typ = typ, ir_name = n', attrs = attrs })
       ir_t <- tmap typ
       return $ (sseq p (IR.Assign (IR.LVar n') ir),
                 IR.DeclareVar n' ir_t)

tr_binop' A.Plus  = do return (A.Int,  A.Int,  A.Int,  IR.Plus)
tr_binop' A.Minus = do return (A.Int,  A.Int,  A.Int,  IR.Minus)
tr_binop' A.Div   = do return (A.Int,  A.Int,  A.Int,  IR.Div)
tr_binop' A.Prod  = do return (A.Int,  A.Int,  A.Int,  IR.Prod)
tr_binop' A.Mod   = do return (A.Int,  A.Int,  A.Int,  IR.Mod)
tr_binop' A.Eq    = do return (A.Bool, A.Int,  A.Int , IR.Eq)
tr_binop' A.And   = do return (A.Bool, A.Bool, A.Bool, IR.And)
tr_binop' A.Or    = do return (A.Bool, A.Bool, A.Bool, IR.Or)
tr_binop' A.Band  = do return (A.Bits, A.Bits, A.Bits, IR.Band)
tr_binop' A.Bor   = do return (A.Bits, A.Bits, A.Bits, IR.Bor)
tr_binop' A.BConcat = do return (A.Bits, A.Bits, A.Bits, IR.BConcat)
tr_binop' A.Xor   = do return (A.Bits, A.Bits, A.Bits, IR.Xor)

tr_unop'  A.Neg   = do return (A.Int,  A.Int,  IR.Neg)
tr_unop'  A.Not   = do return (A.Bool, A.Bool, IR.Not)
tr_unop'  A.Bnot  = do return (A.Bits, A.Bits, IR.Bnot)

tr_binop i op l r =
    do (l_p, l_t, l_ir) <- tr_expr' i l
       (r_p, r_t, r_ir) <- tr_expr' i r
       (e_t, l_tt, r_tt, op_ir) <- tr_binop' op
       abortIf (not $ tmatch l_tt l_t) "Error on binop, left operand"
       abortIf (not $ tmatch r_tt r_t) "Error on binop, right operand"
       return (sseq l_p r_p, e_t, IR.BinOp op_ir l_ir r_ir)

tr_unop i op e =
    do (e_p, e_t, e_ir) <- tr_expr' i e
       (r_t, e_tt, op_ir) <- tr_unop' op
       abortIf (not $ tmatch e_tt e_t) "Error on unop"
       return (e_p, r_t, IR.UnOp op_ir e_ir)

tr_save :: TM (IR.Stmt, A.Type, IR.Expr) -> TM (IR.Stmt, A.Type, IR.Expr)
tr_save m =
    do (p, t, e) <- m
       case t of
           A.Bits ->
               do t' <- tmap t
                  v <- fresh t'
                  return (sseq p (IR.Assign v e), t, IR.LV v)
           _ ->
               do return (p, t, e)

tr_call i f args =
    do abortIf i "Can't call functions in global initializers"
       d <- env_lookup f
       let A.Fun expected_t ret = typ d

       as <- mapM (tr_expr' i) args
       let (args_prep, actual_t, args_ir) = unzip3 as

       abortIf (length actual_t /= length expected_t)
           "Wrong number of arguments on function call"

       let ok = zipWith tmatch actual_t expected_t
       abortIf (not (all id ok))
           "Ill typed function argument on call"

       return (foldl sseq IR.Skip args_prep, ret, IR.Call (ir_name d) args_ir)

tr_slice i a f t =
    do (a_p, a_t, a_ir) <- tr_expr' i a
       (f_p, f_t, f_ir) <- tr_expr' i f
       (t_p, t_t, t_ir) <- tr_expr' i t
       case a_t of
           A.Bits -> return ()
           x -> abort "Slices can only be used on bitseqs"

       abortIf (not $ tmatch f_t A.Int)
           "Slice's 'from' has to be of type int"
       abortIf (not $ tmatch t_t A.Int)
           "Slice's 'to' has to be of type int"

       return (sseq (sseq a_p f_p) t_p,
               A.Bits,
               IR.Slice a_ir f_ir t_ir)
