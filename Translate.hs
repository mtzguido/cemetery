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
prepFold ls = sfold $ map prep ls

add_builtins =
    do mapM (uncurry addToEnv) builtins

-- These functions take care of name clashes,
-- the first one fails, while the second one
-- tries to find a similar unused name (TODO)

requestName s =
    do return ()
requestSimilar s =
    do return s

fromLV (IR.LV lv) = lv
fromLV _ = error "internal error, fromLV on non-lv value!"

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

tr_arg (n, t) =
    do n' <- requestSimilar n
       t' <- tmap t

       let s = eseman { typ = t, expr = IR.LV $ IR.LVar n', attrs = [RO] }

       addToEnv n s
       return (IR.Skip, n', t')

translate1 :: A.Decl -> TM IR.Unit
translate1 d@(A.VarDecl _ _ _ _) =
    do d' <- tr_gdecl d
       return $ IR.Decl d'

translate1 (A.FunDecl {A.name = name, A.ret = ret, A.mods = mods,
                       A.args = args, A.body = body}) =
    do ir_ret <- tmap ret
       requestName name

       let s = eseman { typ = fun_t args ret, expr = IR.LV $ IR.LVar name }
       addToEnv name s

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

tr_assign d s =
    do let e = case (typ s, expr s) of
                 (A.Bits, IR.LV lv) -> IR.Copy lv
                 (_, e)             -> e

       return $ IR.Assign (fromLV $ expr d) e

tr_stmt :: A.Stmt -> TM IR.Stmt
tr_stmt (A.Err s) =
    do return $ IR.Error s

tr_stmt A.Skip =
    do return IR.Skip

tr_stmt (A.Assign v e) =
    do vs <- tr_lv False v
       es <- tr_expr e

       abortIf (elem RO (attrs vs)) "Can't assign to const"
       abortIf (not (tmatch (typ es) (typ vs))) "Type mismatch in assignment"

       s' <- tr_assign vs es
       return $ sseq (prep es) s'

tr_stmt (A.Return e) =
    do rt <- getRetType
       es <- tr_expr e
       abortIf (not (tmatch rt (typ es))) "Invalid return"
       es' <- save es

       return $ sfold [prep es', IR.Return (expr es')]

tr_stmt (A.Seq l r) =
    do ll <- tr_stmt l
       rr <- tr_stmt r
       return $ sseq ll rr

tr_stmt (A.Decl d) =
    do (p, d') <- tr_ldecl d
       addDecl d'
       return p

tr_stmt (A.If c t e) =
    do cs <- tr_expr c
       abortIf (not (tmatch (typ cs) A.Bool))
           "If conditions have to be of type Bool"
       tt <- tr_body t
       ee <- tr_body e
       return $ sseq (prep cs) (IR.If (expr cs) tt ee)

tr_stmt (A.For v f t b) =
    do it <- fresh IR.Int
       f_save <- fresh IR.Int
       t_save <- fresh IR.Int
       fs <- tr_expr f
       ts <- tr_expr t

       abortIf (not $ tmatch (typ fs) A.Int)
           "For bounds have to be of type Int (low)"
       abortIf (not $ tmatch (typ ts) A.Int)
           "For bounds have to be of type Int (high)"

       let prep_f = IR.Assign f_save (expr fs)
       pushLevel

       let s = eseman {typ = A.Int, attrs = [RO], expr = IR.LV it}

       addToEnv v s
       b' <- tr_body b
       popLevel

       return $ sfold [prep fs,
                       IR.Assign f_save (expr fs),
                       prep ts,
                       IR.Assign t_save (expr ts),
                       IR.For it (IR.LV f_save) (IR.LV t_save) b'
                      ]

-- The bool represents wether we're translating a global initializer, so
-- function calls are prohibited.
tr_expr = tr_expr' False
tr_init = tr_expr' True

tr_expr' i e =
    do s <- tr_expr'' i e
       csave s

tr_expr'' :: Bool -> A.Expr -> TM ExprSeman
tr_expr'' i (A.ConstInt ii) =
    do return $ eseman { typ = A.Int, expr = IR.ConstInt ii }

tr_expr'' i (A.ConstBool b) =
    do return $ eseman { typ = A.Bool, expr = IR.ConstBool b }

tr_expr'' i (A.BinOp op l r) =
    do tr_binop i op l r

tr_expr'' i (A.UnOp op e) =
    do tr_unop i op e

tr_expr'' i (A.LV lv) =
    do tr_lv i lv

tr_expr'' i (A.Call f args) =
    do tr_call i f args

tr_expr'' i (A.Arr es) =
    do ess <- mapM (tr_expr'' i) es
       let t = typ (head ess)
           types = map typ  ess
           preps = map prep ess
           exprs = map expr ess

       abortIf (not $ all (==t) (tail types))
         "All elements of the array need to have the same type"

       return $ eseman { prep = sfold preps,
                         typ = A.ArrT t (Just (length es)),
                         expr = IR.Arr exprs
                       }

tr_expr'' i (A.Slice a f t) =
    do tr_slice i a f t

tr_expr'' i (A.ConstFloat _) =
    do abort "Floats unsupported"

tr_expr'' i (A.ConstStr _) =
    do abort "Strings unsupported"

tr_expr'' i (A.BinLit b s) =
    do abortIf i "Binary literals not supported as global initiliazers"
       return $ eseman { typ = A.Bits, expr = IR.ConstBits b s }

tmap :: A.Type -> TM IR.Type
tmap A.Int  = do return IR.Int
tmap A.Bool = do return IR.Bool
tmap A.Bits = do return IR.Bits

tmap (A.ArrT t l) =
    do t' <- tmap t
       return (IR.ArrT t' l)

tmap t =
    do abort $ "Can't map that type (" ++ (show t) ++ ")"

-- Declaration translation

tr_gdecl (A.VarDecl n mods _      Nothing) =
    do abort "Global constants need an initializer"

tr_gdecl (A.VarDecl n mods (Just t) (Just e)) =
    do es <- tr_init e
       abortIf (prep es /= IR.Skip) "Internal error (non-empty prep in global context)"
       abortIf (not (tmatch (typ es) t)) "Type and initializer don't match"
       tr_gdecl' n mods (typ es) (expr es)

tr_gdecl (A.VarDecl n mods Nothing  (Just e)) =
    do es <- tr_init e
       abortIf (prep es /= IR.Skip) "Internal error (non-empty prep in global context)"
       tr_gdecl' n mods (typ es) (expr es)

tr_gdecl' :: String -> [A.Mods] -> A.Type -> IR.Expr -> TM IR.Decl
tr_gdecl' n mods typ ir =
    do requestName n
       abortIf (not (elem A.Const mods))
           "Global variables can only be constants"

       let s = eseman { typ = typ, expr = IR.LV $ IR.LVar n,
                        attrs = [RO] }

       addToEnv n s
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
       i <- default_initializer t
       let s = eseman { typ = t, expr = i }
       tr_ldecl' n mods s

tr_ldecl (A.VarDecl n mods (Just t) (Just e)) =
    do es <- tr_expr e
       abortIf (not (tmatch (typ es) t)) "Type and initializer don't match"
       tr_ldecl' n mods es

tr_ldecl (A.VarDecl n mods Nothing  (Just e)) =
    do es <- tr_expr e
       tr_ldecl' n mods es

tr_ldecl' :: String -> [A.Mods] -> ExprSeman -> TM (IR.Stmt, IR.Decl)
tr_ldecl' n mods es =
    do n' <- requestSimilar n

       let attrs = if elem A.Const mods
                   then [RO]
                   else []

       abortIf (elem A.Extern mods) "External on local scope?"

       let d = eseman { typ = typ es, expr = IR.LV $ IR.LVar n', attrs = attrs }
       addToEnv n d
       ir_t <- tmap (typ es)

       s <- tr_assign d es

       return $ (sseq (prep es) s, IR.DeclLocal (IR.LVar n') ir_t)

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
    (A.Band,    A.Bits, A.Bits,   A.Bits, IR.Band,    True ),
    (A.Bor,     A.Bits, A.Bits,   A.Bits, IR.Bor,     True ),
    (A.BConcat, A.Bits, A.Bits,   A.Bits, IR.BConcat, False),
    (A.Xor,     A.Bits, A.Bits,   A.Bits, IR.Xor,     True ),
    (A.LShift,  A.Bits, A.Int,    A.Bits, IR.LShift,  False),
    (A.RShift,  A.Bits, A.Int,    A.Bits, IR.RShift,  False),
    (A.LRot,    A.Bits, A.Int,    A.Bits, IR.LRot,    False),
    (A.RRot,    A.Bits, A.Int,    A.Bits, IR.RRot,    False),
    (A.Le,      A.Int,  A.Int,    A.Bool, IR.Le,      False),
    (A.Lt,      A.Int,  A.Int,    A.Bool, IR.Lt,      False),
    (A.Ge,      A.Int,  A.Int,    A.Bool, IR.Ge,      False),
    (A.Gt,      A.Int,  A.Int,    A.Bool, IR.Gt,      False)
 ]

unop_table :: [(A.UnOp, A.Type, A.Type, IR.UnOp, Bool)]
unop_table = [
    (A.Neg,   A.Int , A.Int ,  IR.Neg,    False),
    (A.Not,   A.Bool, A.Bool,  IR.Not,    False),
    (A.Bnot,  A.Bits, A.Bits,  IR.Bnot,   True )
 ]

tr_binop i op l r =
    do ls <- tr_expr'' i l
       rs <- tr_expr'' i r
       let ops = filter (\(a,b,c,_,_,_) -> (a,b,c) == (op, typ ls, typ rs)) binop_table
       (e_t, op_ir, clusterable) <-
           case ops of
              [] -> abort $ "Error on binop, no suitable operator found: " ++
                            show (typ ls, op, typ rs)
              [(_,_,_,d,e,f)] -> return (d, e, f)
              _ -> abort $ "Internal error, more than one binop match"

       if clusterable
       then tr_cluster_bin e_t op_ir ls rs
       else do ls <- csave ls
               rs <- csave rs
               return $ eseman { prep = prepFold [ls, rs],
                                 typ = e_t,
                                 expr = IR.BinOp op_ir (expr ls) (expr rs) }

tr_unop i op e =
    do es <- tr_expr'' i e
       let ops = filter (\(a,b,_,_,_) -> (a,b) == (op, typ es)) unop_table
       (r_t, op_ir, clusterable) <-
           case ops of
               [] -> abort $ "Error on unop, no suitable operator found: " ++
                             show (op, typ es)
               [(_,_,c,d,e)] -> return (c, d, e)
               _ -> abort $ "Internal error, more than one unop match"

       if clusterable
       then tr_cluster_un r_t op_ir es
       else do es <- csave es
               return $ eseman { prep = prep es,
                                 typ = r_t,
                                 expr = IR.UnOp op_ir (expr es) }

clusterize s | IR.Cluster _ _ <- expr s =
    do return s

clusterize s =
    do ss <- csave s
       let e' = IR.Cluster (IR.CArg 0) [(fromLV (expr ss), False)]
       return $ ss { expr = e' }

tr_cluster_bin t op ls rs =
    do ls <- clusterize ls
       rs <- clusterize rs
       return $ eseman { prep = prepFold [ls, rs],
                         typ = t,
                         expr = IR.c_binop op (expr ls) (expr rs) }

tr_cluster_un t op es =
    do es <- clusterize es
       return $ eseman { prep = prep es,
                         typ = t,
                         expr = IR.c_unop op (expr es) }

save' :: ExprSeman -> TM ExprSeman
save' s =
    do t' <- tmap (typ s)
       r <- fresh t'
       return $ s { prep = sseq (prep s) (IR.Assign r (expr s)),
                    expr = IR.LV r }

save :: ExprSeman -> TM ExprSeman
save s | IR.LV (IR.Access _ _) <- expr s =
    do save' s

save s | IR.LV _ <- expr s =
    do return s

save s =
    do save' s

csave :: ExprSeman -> TM ExprSeman
csave s =
    do if typ s == A.Bits
       then save   s
       else return s

tr_call i f args =
    do abortIf i "Can't call functions in global initializers"
       d <- env_lookup f
       A.Fun expected_t ret <- case typ d of
                                   A.Fun _ _ -> return (typ d)
                                   _ -> abort $ f ++ ": is not a function"

       as <- mapM (tr_expr' i) args
       let args_prep = map prep as
           actual_t  = map typ  as
           args_ir   = map expr as

       abortIf (length actual_t /= length expected_t)
           "Wrong number of arguments on function call"

       let ok = zipWith tmatch actual_t expected_t
       abortIf (not (all id ok))
           "Ill typed function argument on call"

       return $ eseman { prep = sfold (prep d : args_prep),
                         typ = ret,
                         expr = IR.Call (fromLV $ expr d) args_ir }

tr_slice i a f t =
    do as <- tr_expr' i a
       fs <- tr_expr' i f
       ts <- tr_expr' i t
       case typ as of
           A.Bits -> return ()
           x -> abort "Slices can only be used on bitseqs"

       abortIf (not $ tmatch (typ fs) A.Int)
           "Slice's 'from' has to be of type int"
       abortIf (not $ tmatch (typ ts) A.Int)
           "Slice's 'to' has to be of type int"

       return $ eseman { prep = prepFold [as, fs, ts],
                         typ = A.Bits,
                         expr = IR.Slice (fromLV $ expr as) (expr fs) (expr ts) }

tr_lv i (A.Var n) =
    do env_lookup n

tr_lv i (A.Access a j) =
    do as <- tr_lv i a
       js <- tr_expr'' i j
       t <- case typ as of
                A.ArrT e _ -> return e
                _ -> abort "Accesses can only be used on arrays"

       abortIf (not $ tmatch (typ js) A.Int)
           "Access index has to be of type \"int\""

       return $ eseman { prep = prepFold [as, js],
                         typ = t,
                         expr = IR.LV $ IR.Access (fromLV $ expr as) (expr js) }
