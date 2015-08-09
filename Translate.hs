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

sseq = IR.sseq
sfold = IR.sfold

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

translate1 (A.FunDecl {A.name = name, A.ret = ret, A.mods = mods,
                       A.args = args, A.body = body}) =
    do ir_ret <- tmap ret
       requestName name
       addToEnv name (envv { typ = fun_t args ret, ir_lv = IR.LVar name })

       let tr_arg (s, t) = do s' <- requestSimilar s
                              t' <- tmap t
                              r <- fresh t'
                              let d = envv { typ = t, ir_lv = r }
                              p <- tr_assign d t (IR.LV $ IR.LVar s')
                              addToEnv s d
                              return (p, s', t')

       pushLevel
       args' <- mapM tr_arg args
       let ir_args = map (\(a,b,c) -> (b,c)) args'
       let prep_args = sfold $ map (\(a,b,c) -> a) args'

       setRetType ret
       ir_body_s <- tr_stmt body
       l <- popLevel

       let ir_mods = if elem A.Static mods then [IR.Static] else []

       let ft = IR.Funtype { IR.name = name,
                             IR.args = ir_args,
                             IR.mods = ir_mods,
                             IR.ret = ir_ret }
       return $ IR.FunDef ft (reverse $ decls l, sseq prep_args ir_body_s)

tr_body :: A.Stmt -> TM IR.Block
tr_body b = do pushLevel
               s <- tr_stmt b
               l <- popLevel
               return (reverse $ decls l, s)

tr_assign d typ ir =
    do let expr = case typ of
                       A.Bits -> case ir of
                                     IR.LV _ -> IR.Copy ir
                                     _ -> ir
                       _ -> ir

       return $ IR.Assign (ir_lv d) expr

tr_stmt :: A.Stmt -> TM IR.Stmt
tr_stmt (A.Err s) =
    do return $ IR.Error s

tr_stmt A.Skip =
    do return IR.Skip

tr_stmt (A.Assign n e) =
    do d <- env_lookup n
       (p_e, t_e, ir_e) <- tr_expr e

       abortIf (elem RO (attrs d)) "Can't assign to const"
       abortIf (not (tmatch t_e (typ d))) "Type mismatch in assignment"

       s' <- tr_assign d t_e ir_e
       return $ sseq p_e s'

tr_stmt (A.Return e) =
    do rt <- getRetType
       (p_e, t_e, ir_e) <- tr_expr e
       abortIf (not (tmatch rt t_e)) "Invalid return"
       case ir_e of
           IR.LV _ -> return $ sseq p_e (IR.Return ir_e)
           _       -> do ir_t <- tmap rt
                         t <- fresh ir_t
                         return $ sfold [p_e, IR.Assign t ir_e, IR.Return (IR.LV t)]

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

tr_stmt (A.For v f t b) =
    do it <- fresh IR.Int
       f_save <- fresh IR.Int
       t_save <- fresh IR.Int
       (f_p, f_t, f_ir) <- tr_expr f
       (t_p, t_t, t_ir) <- tr_expr t

       abortIf (not $ tmatch f_t A.Int)
           "For bound have to be of type Int (low)"
       abortIf (not $ tmatch t_t A.Int)
           "For bound have to be of type Int (high)"

       let prep_f = IR.Assign f_save f_ir
       pushLevel
       addToEnv v (envv {typ = A.Int, attrs = [RO], ir_lv = it})
       b' <- tr_body b
       popLevel

       return $ sfold [f_p,
                       IR.Assign f_save f_ir,
                       t_p,
                       IR.Assign t_save t_ir,
                       IR.For it (IR.LV f_save) (IR.LV t_save) b'
                      ]


-- The bool represents wether we're translating a global initializer, so
-- function calls are prohibited.
tr_expr = tr_expr' False
tr_init = tr_expr' True

tr_expr' i e =
    do s <- tr_expr'' i e
       csave s

tr_expr'' :: Bool -> A.Expr -> TM (IR.Stmt, A.Type, IR.Expr)
tr_expr'' i (A.ConstInt ii) =
    do return (IR.Skip, A.Int, IR.ConstInt ii)

tr_expr'' i (A.ConstBool b) =
    do return (IR.Skip, A.Bool, IR.ConstBool b)

tr_expr'' i (A.BinOp op l r) =
    do tr_binop i op l r

tr_expr'' i (A.UnOp op e) =
    do tr_unop i op e

tr_expr'' i (A.Var n) =
    do d <- env_lookup n
       return (IR.Skip, typ d, IR.LV (ir_lv d))

tr_expr'' i (A.Call f args) =
    do tr_call i f args

tr_expr'' i (A.Arr es) =
    do (es_preps, es_types, es_irs) <- liftM unzip3 $ mapM (tr_expr'' i) es
       abortIf (not $ all (== head es_types) es_types)
         "All elements of the array need to have the same type"

       return (sfold es_preps,
               A.ArrT (head es_types),
               IR.Arr es_irs)

tr_expr'' i (A.Slice a f t) =
    do tr_slice i a f t

tr_expr'' i (A.Access a idx) =
    do (a_p, a_t, a_ir) <- tr_expr'' i a
       (i_p, i_t, i_ir) <- tr_expr'' i idx
       t <- case a_t of
                A.ArrT e -> return e
                _ -> abort "Accesses can only be used on arrays"

       abortIf (not $ tmatch i_t A.Int)
           "Access index has to be of type int"

       return (sseq a_p i_p,
               t,
               IR.Access a_ir i_ir)

tr_expr'' i (A.ConstFloat _) =
    do abort "Floats unsupported"

tr_expr'' i (A.ConstStr _) =
    do abort "Strings unsupported"

tr_expr'' i (A.BinLit b s) =
    do abortIf i "Binary literals not supported as global initiliazers"
       return (IR.Skip, A.Bits, IR.ConstBits b s)

tmap :: A.Type -> TM IR.Type
tmap A.Int  = do return IR.Int
tmap A.Bool = do return IR.Bool
tmap A.Bits = do return IR.Bits

tmap (A.ArrT t) =
    do t' <- tmap t
       return (IR.ArrT t')

tmap t =
    do abort $ "Can't map that type (" ++ (show t) ++ ")"

isArr (A.ArrT _) = True
isArr _ = False

-- Declaration translation

tr_gdecl (A.VarDecl n mods _      Nothing) =
    do abort "Global constants need an initializer"

tr_gdecl (A.VarDecl n mods (Just t) (Just e)) =
    do (prep, e_t, e_ir) <- tr_init e
       abortIf (prep /= IR.Skip) "Internal error"
       abortIf (not (tmatch e_t t)) "Type and initializer don't match"
       tr_gdecl' n mods e_t e_ir

tr_gdecl (A.VarDecl n mods Nothing  (Just e)) =
    do (prep, e_t, e_ir) <- tr_init e
       abortIf (prep /= IR.Skip) "Internal error"
       tr_gdecl' n mods e_t e_ir

tr_gdecl' :: String -> [A.Mods] -> A.Type -> IR.Expr -> TM IR.Decl
tr_gdecl' n mods typ ir =
    do requestName n
       abortIf (not (elem A.Const mods))
           "Global variables can only be constants"

       addToEnv n (envv { typ = typ, ir_lv = IR.LVar n, attrs = [RO] })
       ir_t <- tmap typ

       -- At this point, we'll need to simplify ir
       -- to a "static" form, since it's allowed
       -- to initialize a variable with || (concatenation)
       -- (once that's done) but that will likely result in a
       -- function call. Either reduce everything or prepare
       -- to do so in a cmt_init().
       return $ IR.DeclGlobal n ir_t ir

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

tr_ldecl' :: String -> [A.Mods] -> IR.Stmt -> A.Type -> IR.Expr
          -> TM (IR.Stmt, IR.Decl)
tr_ldecl' n mods p typ ir =
    do n' <- requestSimilar n

       abortIf (isArr typ) "arrays can't be declared locally, only as global constants"

       let attrs = if elem A.Const mods
                   then [RO]
                   else []

       abortIf (elem A.Extern mods) "External on local scope?"

       let d = envv { typ = typ, ir_lv = IR.LVar n', attrs = attrs }
       addToEnv n d
       ir_t <- tmap typ

       s <- tr_assign d typ ir

       return $ (sseq p s, IR.DeclLocal (IR.LVar n') ir_t)

binop_table :: [(A.BinOp, A.Type, A.Type, A.Type, IR.BinOp, Bool)]
binop_table = [
    (A.Plus,    A.Int,  A.Int,    A.Int,  IR.Plus,    False),
    (A.Plus,    A.Bits, A.Bits,   A.Bits, IR.ModPlus, False),
    (A.Minus,   A.Int,  A.Int,    A.Int,  IR.Minus,   False),
    (A.Div,     A.Int,  A.Int,    A.Int,  IR.Div,     False),
    (A.Prod,    A.Int,  A.Int,    A.Int,  IR.Prod,    False),
    (A.Mod,     A.Int,  A.Int,    A.Int,  IR.Mod,     False),
    (A.Eq,      A.Int,  A.Int,    A.Bool, IR.Eq,      False),
    (A.Eq,      A.Bits, A.Bits,   A.Bool, IR.BitEq,   False),
    (A.And,     A.Bool, A.Bool,   A.Bool, IR.And,     False),
    (A.Or,      A.Bool, A.Bool,   A.Bool, IR.Or,      False),
    (A.Band,    A.Bits, A.Bits,   A.Bits, IR.Band,    False),
    (A.Bor,     A.Bits, A.Bits,   A.Bits, IR.Bor,     False),
    (A.BConcat, A.Bits, A.Bits,   A.Bits, IR.BConcat, False),
    (A.Xor,     A.Bits, A.Bits,   A.Bits, IR.Xor,     False),
    (A.LShift,  A.Bits, A.Int,    A.Bits, IR.LShift,  False),
    (A.RShift,  A.Bits, A.Int,    A.Bits, IR.RShift,  False),
    (A.LRot,    A.Bits, A.Int,    A.Bits, IR.LRot,    False),
    (A.RRot,    A.Bits, A.Int,    A.Bits, IR.RRot,    False),
    (A.Le,      A.Int,  A.Int,    A.Bool, IR.Le,      False),
    (A.Lt,      A.Int,  A.Int,    A.Bool, IR.Lt,      False),
    (A.Ge,      A.Int,  A.Int,    A.Bool, IR.Ge,      False),
    (A.Gt,      A.Int,  A.Int,    A.Bool, IR.Gt,      False)
 ]

tr_unop'  A.Neg   = do return (A.Int,  A.Int,  IR.Neg)
tr_unop'  A.Not   = do return (A.Bool, A.Bool, IR.Not)
tr_unop'  A.Bnot  = do return (A.Bits, A.Bits, IR.Bnot)

tr_binop i op l r =
    do ls@(l_p, l_t, l_ir) <- tr_expr'' i l
       rs@(r_p, r_t, r_ir) <- tr_expr'' i r
       let ops = filter (\(a,b,c,_,_,_) -> (a,b,c) == (op, l_t, r_t)) binop_table
       (e_t, op_ir, clusterable) <-
           case ops of
              [] -> abort $ "Error on binop, no suitable operator found: " ++
                            show (l_t, op, r_t)
              [(_,_,_,d,e,f)] -> return (d, e, f)
              _ -> abort $ "Internal error, more than one operator match"

       if clusterable
       then tr_cluster e_t op_ir ls rs
       else do ls@(l_p, _, l_ir) <- csave ls
               rs@(r_p, _, r_ir) <- csave rs
               return (sseq l_p r_p, e_t, IR.BinOp op_ir l_ir r_ir)

clusterize s@(p, t, IR.Cluster _ _) =
    do return s

clusterize (p, t, e) =
    do let e' = IR.Cluster (IR.CArg 0) [e]
       return (p, t, e')

tr_cluster t op ls rs =
    do ls@(l_p, _, l_ir) <- clusterize ls
       rs@(r_p, _, r_ir) <- clusterize rs
       return (sseq l_p r_p, t, IR.c_binop op l_ir r_ir)

tr_unop i op e =
    do (e_p, e_t, e_ir) <- tr_expr' i e
       (r_t, e_tt, op_ir) <- tr_unop' op
       abortIf (not $ tmatch e_tt e_t) "Error on unop"
       return (e_p, r_t, IR.UnOp op_ir e_ir)

save :: (IR.Stmt, A.Type, IR.Expr) -> TM (IR.Stmt, A.Type, IR.Expr)
save e@(p, t, IR.LV _) =
    do return e

save (p, t, e) =
    do t' <- tmap t
       r <- fresh t'
       return (sseq p (IR.Assign r e), t, IR.LV r)

csave :: (IR.Stmt, A.Type, IR.Expr) -> TM (IR.Stmt, A.Type, IR.Expr)
csave (p, t, e) =
    do if t == A.Bits
       then save   (p, t, e)
       else return (p, t, e)

tr_call i f args =
    do abortIf i "Can't call functions in global initializers"
       d <- env_lookup f
       A.Fun expected_t ret <- case typ d of
                                   A.Fun _ _ -> return (typ d)
                                   _ -> abort $ f ++ ": is not a function"

       as <- mapM (tr_expr' i) args
       let (args_prep, actual_t, args_ir) = unzip3 as

       abortIf (length actual_t /= length expected_t)
           "Wrong number of arguments on function call"

       let ok = zipWith tmatch actual_t expected_t
       abortIf (not (all id ok))
           "Ill typed function argument on call"

       return (sfold args_prep, ret, IR.Call (ir_lv d) args_ir)

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

       return (sfold [a_p, f_p, t_p],
               A.Bits,
               IR.Slice a_ir f_ir t_ir)
