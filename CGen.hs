module CGen where

import Data.List
import Data.Maybe
import Data.Monoid
import qualified IR as I
import qualified CLang as C
import Common
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

bitsType_str = C.Custom "struct cmt_init"
bitsType = C.Custom "cmt_bits_t"
bitsType_ptr = C.Custom "cmt_bits_t*"
wordType = C.Custom "word_t"

c_assign l e = C.Expr $ C.BinOp C.Assign l e
c_call f a = C.Expr $ C.Call f a

-- Generate source file and header
cgen :: I.IR -> (C.Prog, C.Prog)
cgen ir = let (units, ()) = runGM (g_ir ir)
              c = C.Prog { C.includes = ["stdbool", "stdlib", "stdio",
                                         "stddef", "string", "stdarg"],
                           C.units = units
                         }
              h = C.Prog { C.includes = ["stdbool"],
                           C.units = concat $ map header_unit units
                         }
           in (c, h)

-- In 'extra' we keep those functions that are generated
-- despite not being an IR unit, like the implementations
-- for clusters.
data CGenState =
    CGenState { globals :: [C.Decl],
                extra :: [C.Unit],
                buflit_counter :: Int,
                cluster_counter :: Int,
                clusters_generated :: M.Map (I.ClusterExpr, [Bool]) String
              }

header_unit (C.Decl d) = []
header_unit (C.FunDef ft _) = if elem C.Static (C.mods ft)
                              then []
                              else [C.FunDecl ft]
-- Cemetery shouldn't generate any FunDecls on the source file

initState =
    CGenState { globals = [],
                extra = [],
                buflit_counter = 0,
                cluster_counter = 0,
                clusters_generated = M.empty
              }

type GM = StateT CGenState (
           WriterT [C.Unit] (
            Identity
           ))

add_gdecl :: C.Decl -> GM ()
add_gdecl d =
    do s <- get
       put (s { globals = globals s ++ [d]})

add_extra :: C.Unit -> GM ()
add_extra f =
    do s <- get
       put (s { extra = extra s ++ [f]})

add_cluster :: (I.ClusterExpr, [Bool]) -> String -> GM ()
add_cluster k v =
    do s <- get
       put (s { clusters_generated = M.insert k v (clusters_generated s)})

get_cluster_idx =
    do s <- get
       put (s { cluster_counter = cluster_counter s + 1 })
       return (cluster_counter s)

fresh_buflit_counter =
    do s <- get
       put (s { buflit_counter = buflit_counter s + 1})
       return (buflit_counter s)

sseq C.Skip r = r
sseq l C.Skip = l
sseq l r = C.Seq l r

sfold = foldl sseq C.Skip

runGM :: GM t -> ([C.Unit], t)
runGM m = let m' = runStateT m initState
              m'' = runWriterT m'
              ((r, s), units) = runIdentity m''
              gdecls = map C.Decl (globals s)
           in (gdecls ++ extra s ++ units, r)

g_ir :: I.IR -> GM ()
g_ir p = do bs <- mapM g_unit p
            return ()

g_unit :: I.Unit -> GM ()
g_unit (I.Decl d) =
    do d' <- g_decl d
       tell [C.Decl d']

g_unit (I.FunDef (I.Funtype { I.name = name,
                              I.args = args,
                              I.mods = mods,
                              I.ret  = ret}) body) =
    do c_args <- mapM g_arg args
       c_ret <- g_type ret
       c_body <- g_body body
       c_mods <- mapM g_mods mods
       let ft = C.Funtype { C.name = name, C.args = c_args,
                            C.mods = c_mods, C.ret = c_ret }
       tell [C.FunDef ft c_body]

g_mods I.Static = do return C.Static

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
g_decl (I.DeclLocal (I.Temp i) t) =
    do let v = "t" ++ show i
       tt <- g_type t
       return $ C.VarDecl v tt Nothing []

g_decl (I.DeclLocal (I.LVar n) t) =
    do tt <- g_type t
       return $ C.VarDecl n tt Nothing []

g_decl (I.DeclGlobal n t e) =
    do tt <- g_type t
       e_c <- g_expr e
       return $ C.VarDecl n tt (Just e_c) []

zero_bits (I.ConstBits [] (I.ConstInt 0)) = True
zero_bits _ = False

g_stmt :: I.Stmt -> GM C.Stmt
g_stmt (I.Seq l r) =
    do ll <- g_stmt l
       rr <- g_stmt r
       return $ sseq ll rr

g_stmt I.Skip =
    do return C.Skip

-- Special handling for array assignments, since C doesn't
-- provide an array assignment
g_stmt (I.Assign lv (I.Arr es)) | all zero_bits es =
    do lv' <- g_lvalue lv
       let n = length es
       return $ (c_call "__cmt_init_bitarr" [C.LV lv', C.ConstInt n])

g_stmt (I.Assign lv (I.Arr es)) =
    do let a i = I.Assign (I.Access lv (I.ConstInt i)) (es !! i)
       ss <- mapM (g_stmt.a) [0..length es - 1]
       return $ sfold ss

g_stmt (I.Assign lv e) =
    do c_e <- g_expr e
       c_lv <- g_lvalue lv
       return (C.Expr $ C.BinOp C.Assign (C.LV c_lv) c_e)

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
       let init = C.BinOp C.Assign (C.LV i) f
       let cond = C.BinOp C.Le (C.LV i) t
       let inc = C.BinOp C.Assign (C.LV i)
                         (C.BinOp C.Plus (C.LV i) (C.ConstInt 1))
       body <- g_body b
       return $ C.For init cond inc body

g_stmt (I.Error s) =
    do let c = C.Call "fprintf" [C.LV (C.LVar "stderr"),
                                 C.ConstStr $ "Cemetery error: " ++ s]
       let a = C.Call "abort" []
       return (C.Seq (C.Expr c) (C.Expr a))

g_stmt (I.Free lvs) =
    do es <- mapM g_free_one lvs
       return (sfold es)

g_stmt (I.FreeArr [a] l) =
    do a' <- g_lvalue a
       return (C.Expr (C.Call "__cmt_free_bitarr" [C.LV a', C.ConstInt l]))

g_free_one l =
    do l' <- g_lvalue l
       return $ C.Expr $ C.Call "cmt_free" [C.LV l']

g_expr :: I.Expr -> GM C.Expr
g_expr (I.ConstInt i) =
    do return $ C.ConstInt i

g_expr (I.ConstBool b) =
    do return $ C.ConstBool b

g_expr (I.BinOp op l r) =
    do ll <- g_expr l
       rr <- g_expr r
       g_binop op ll rr

g_expr (I.IPOp op b s) =
    do bb <- g_lvalue b
       ss <- g_expr s
       g_inplaceop op bb ss

-- Special case for inequalities
g_expr (I.UnOp I.Not (I.BinOp I.Eq l r)) =
    do ll <- g_expr l
       rr <- g_expr r
       return $ C.BinOp C.Neq ll rr

g_expr (I.UnOp op l) =
    do ll <- g_expr l
       g_unop op ll

g_expr (I.LV lv) =
    do lv_c <- g_lvalue lv
       return $ C.LV lv_c

g_expr (I.Call cc args) =
    do c_args <- mapM g_expr args
       let f_name = case cc of
                        I.LVar n -> n
                        I.Builtin b -> builtin_name b

       return $ C.Call f_name c_args

g_expr (I.Arr es) =
    do es' <- mapM g_expr es
       return $ C.Arr es'

g_expr (I.Slice a f t) =
    do aa <- g_lvalue a
       ff <- g_expr f
       tt <- g_expr t
       return $ C.Call "__cmt_slice" [C.LV aa, ff, tt]

g_expr (I.ConstBits b l) | all (==0) b =
    do l' <- g_expr l
       return $ C.Call "__cmt_zero" [l']

g_expr (I.ConstBits b l) =
    do name <- g_const_bits b l
       l' <- g_expr l
       let p = C.UnOp C.Address (C.LV (C.LVar name))
       return $ C.Call "__cmt_init" [p, l']

-- "Copy" is implemented as a function call.
g_expr (I.Copy lv) =
    g_expr (I.Call (I.LVar "__cmt_copy") [I.LV lv])

-- optimize simple clusters
g_expr (I.Cluster (I.CBinOp op (I.CArg m) (I.CArg n)) as)
            | all not (map snd as) =
    do l <- g_lvalue (fst $ as!!m)
       r <- g_lvalue (fst $ as!!n)
       g_binop op (C.LV l) (C.LV r)

g_expr (I.Cluster (I.CUnOp op (I.CArg n)) as)
            | all not (map snd as) =
    do e <- g_lvalue (fst $ as!!n)
       g_unop op (C.LV e)

g_expr (I.Cluster e as) =
    do n <- reg_cluster e (map snd as) (length as)
       as' <- mapM g_lvalue (map fst as)
       return $ C.Call n (map C.LV as')

reg_cluster e fs n =
    do s <- get
       let m = clusters_generated s
       case M.lookup (e,fs) m of
           Just f -> return f
           Nothing -> do c@(C.FunDef ft _) <- make_cluster e fs n
                         add_extra c
                         add_cluster (e,fs) (C.name ft)
                         return (C.name ft)

g_const_bits b l =
    do c <- fresh_buflit_counter
       let name = "__cmt_buf_literal_" ++ show c
       let arr  = map C.ConstInt (reverse b)
       let carr = C.Arr arr
       let str = C.StructVal [("data", carr), ("length", C.ConstInt (length b))]
       add_gdecl (C.VarDecl name bitsType_str (Just str) [C.Static, C.Const])
       return name

g_type I.Int  = do return C.Int
g_type I.Bool = do return C.Bool
g_type I.Bits = do return bitsType
g_type (I.ArrT t l) =
    do t' <- g_type t
       return (C.ArrT t' l)

g_binop I.Plus    l r = do return $ C.BinOp C.Plus  l r
g_binop I.Minus   l r = do return $ C.BinOp C.Minus l r
g_binop I.Div     l r = do return $ C.BinOp C.Div   l r
g_binop I.Prod    l r = do return $ C.BinOp C.Prod  l r
g_binop I.Eq      l r = do return $ C.BinOp C.Eq    l r
g_binop I.Mod     l r = do return $ C.Call "__cmt_mod" [l, r]
g_binop I.And     l r = do return $ C.BinOp C.And   l r
g_binop I.Or      l r = do return $ C.BinOp C.Or    l r
g_binop I.Lt      l r = do return $ C.BinOp C.Lt    l r
g_binop I.Le      l r = do return $ C.BinOp C.Le    l r
g_binop I.Gt      l r = do return $ C.BinOp C.Gt    l r
g_binop I.Ge      l r = do return $ C.BinOp C.Ge    l r
g_binop I.Band    l r = do return $ C.Call "__cmt_band" [l, r]
g_binop I.Bor     l r = do return $ C.Call "__cmt_bor" [l, r]
g_binop I.Xor     l r = do return $ C.Call "__cmt_xor" [l, r]
g_binop I.BConcat l r = do return $ C.Call "__cmt_bconcat" [l, r]
g_binop I.LShift  l r = do return $ C.Call "__cmt_shiftl" [l, r]
g_binop I.RShift  l r = do return $ C.Call "__cmt_shiftr" [l, r]
g_binop I.LRot    l r = do return $ C.Call "__cmt_rotl" [l, r]
g_binop I.RRot    l r = do return $ C.Call "__cmt_rotr" [l, r]
g_binop I.ModPlus l r = do return $ C.Call "__cmt_modplus" [l, r]
g_binop I.BitEq   l r = do return $ C.Call "__cmt_eq" [l, r]

g_unop  I.Neg   e   = do return $ C.UnOp C.NegateNum e
g_unop  I.Not   e   = do return $ C.UnOp C.Not       e
g_unop  I.Bnot  e   = do return $ C.Call "__cmt_bnot" [e]

g_lvalue (I.LVar n) =
    do return $ C.LVar n

g_lvalue (I.Temp i) =
    do return $ C.LVar ("t" ++ show i)

g_lvalue (I.Access a i) =
    do aa <- g_lvalue a
       ii <- g_expr i
       return $ C.Access (C.LV aa) ii

builtin_name I.Permute = "__cmt_permute"
builtin_name I.Length  = "cmt_length"
builtin_name I.ToInt   = "__cmt_toint"
builtin_name I.ToBits  = "__cmt_tobits"

inplace_fun I.LRot   = "__cmt_inplace_rotl"
inplace_fun I.RRot   = "__cmt_inplace_rotr"

g_inplaceop op b s =
    do let f = inplace_fun op
       return $ C.Call f [C.LV b, s]

make_cluster e fs n =
    do idx <- get_cluster_idx
       let arg_name i = "a" ++ show i
       let formals = zip (map arg_name [0..n-1]) (repeat bitsType)

       let arg a = C.LV (C.LVar (arg_name a))
       let length a = C.Call "cmt_length" [arg a]
       let word a i = C.Call "get_word" [arg a, i]

       let (ntemp, p, expr) = make_cluster_expr 0 (\i -> word i (C.LV (C.LVar "i"))) e

       let temp_decl i = [C.VarDecl ("_mpt" ++ show i) wordType (Just (C.ConstInt 0)) [],
                          C.VarDecl ("_mpc" ++ show i) wordType Nothing []]

       let temp_decls = concatMap temp_decl [1..ntemp]

       let ast = cluster_ast arg fs expr n p temp_decls

       let ft = C.Funtype { C.name = "__cmt_cluster_impl_" ++ show idx,
                            C.args = formals, C.mods = [C.Static],
                            C.ret = bitsType }

       return $ C.FunDef ft ast

make_cluster_expr n word (I.CArg i) =
    (n, C.Skip, word i)

make_cluster_expr n word (I.CBinOp I.ModPlus l r) =
    let (nl, pl, l') = make_cluster_expr n  word l
        (nr, pr, r') = make_cluster_expr nl word r
        nn = nr + 1
        c  = C.LV $ C.LVar $ "_mpt" ++ show nn
        cc = C.LV $ C.LVar $ "_mpc" ++ show nn

        -- add_carry is actually a macro, but who cares
        p' = c_assign cc (C.Call "add_carry" [c, l', r'])
        p  = sfold [pl, pr, p']
     in (nr + 1, p, cc)

make_cluster_expr n word (I.CBinOp op l r) =
    let (nl, pl, l') = make_cluster_expr n  word l
        (nr, pr, r') = make_cluster_expr nl word r
        op' = clustered_binop op
     in (nr, C.Seq pl pr, C.BinOp op' l' r')

make_cluster_expr n word (I.CUnOp op e) =
    let (ne, pe, e') = make_cluster_expr n word e
        op' = clustered_unop op
     in (ne, pe, C.UnOp op' e')

do_frees fs n =
    let idxs = filter (fs!!) [0..n-1]
        b i = C.LV (C.LVar ("a" ++ show i))
     in map (\i -> c_call "cmt_free" [b i]) idxs

do_free_cond ret arg =
    C.If (C.BinOp C.Neq arg ret)
         ([], C.Expr $ C.Call "cmt_free" [arg])
         ([], C.Skip)

var n = C.LV (C.LVar n)

-- for (i = lo; i < hi; i++) { body }
c_for i lo hi body =
    C.For (C.BinOp C.Assign i lo)
          (C.BinOp C.Lt i hi)
          (C.BinOp C.Assign i (C.BinOp C.Plus i (C.ConstInt 1)))
          body

cluster_skel decls prep body post =
    ([C.VarDecl "ret" bitsType Nothing [],
      C.VarDecl "i"   C.Int    Nothing [],
      C.VarDecl "l"   C.Int    Nothing []] ++
     decls,
     sfold [prep,
            c_for (var "i") (C.ConstInt 0)
                  (C.BinOp C.Member (var "ret") (var "size")) body,
            post,
            c_call "__cmt_fixup" [var "ret"],
            C.Return (var "ret")]
    )

cluster_ast args fs res n p ds | all not fs =
    let ret  = var "ret"
        l    = var "l"
        body = sfold [p, c_call "set_word" [ret, var "i", res]]
        prep_one i = expand_len (args i) l
        prep' = sfold $ map prep_one [0..n-1]
        set_ret = c_assign ret (C.Call "__cmt_alloc" [l])
        prep = sfold [c_assign l (C.ConstInt 0), prep', set_ret]
     in cluster_skel ds prep ([], body) (sfold $ do_frees fs n)

cluster_ast args fs res n p ds =
    let ret    = var "ret"
        l      = var "l"
        u      = var "u"
        length = var "length"
        first_idx = fromJust $ elemIndex True fs
        first = args first_idx
        l_init = C.BinOp C.Member first length
        prep_one a = sseq (expand_u a ret u)
                          (expand_len a l)

        free_args = map args (elemIndices True fs)

        prep' = sfold $ map prep_one (free_args \\ [first]) -- skip the one used for init

        body = sfold [p, c_call "set_word" [ret, var "i", res]]
        set_ret_1 = c_assign ret (C.Call "__cmt_resize_zero" [ret, l])
        set_ret_2 = c_assign (C.UnOp C.Deref u) ret
        u_decl = C.VarDecl "u" bitsType_ptr (Just (C.UnOp C.Address first)) []
        frees = map (do_free_cond ret) free_args

        prep = sfold [c_assign ret first,
                      c_assign l l_init,
                      prep',
                      set_ret_1,
                      set_ret_2]
     in cluster_skel (u_decl:ds) prep ([],body) (sfold frees)

expand_len arg l =
    let length = var "length"
     in C.If (C.BinOp C.Gt (C.BinOp C.Member arg length) l)
             ([], c_assign l (C.BinOp C.Member arg length))
             ([], C.Skip)

expand_u arg ret u =
    let length = var "length"
     in C.If (C.BinOp C.Gt (C.BinOp C.Member arg length)
                           (C.BinOp C.Member ret length))
             ([], C.Seq (c_assign ret arg)
                        (c_assign u (C.UnOp C.Address arg)))
             ([], C.Skip)

clustered_binop I.Band = C.Band
clustered_binop I.Bor  = C.Bor
clustered_binop I.Xor  = C.Xor

clustered_unop  I.Bnot = C.Bnot
