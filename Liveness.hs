module Liveness where

import IR
import qualified Data.Set as S
import Debug.Trace
import Common

---------------------------------------------------------------------
-- Live variable analysis
---------------------------------------------------------------------

liveness (d, s) = (d, sfold $ liv (locals $ filter tracked_decl d) S.empty (flatten s))

locals ds = concatMap ff ds where
         ff (DeclLocal lv _) = [lv]
         ff _ = []

-- Does any of the declarations in ds hide lv?
hidden lv ds =
    [] /= filter (\d -> case d of DeclLocal lv' _ -> lv == lv'
                                  _ -> False) ds

-- Is the variable lv read in this chunk, before being written?
used_e :: LValue -> Expr -> Bool
used_e lv (ConstInt _) = False
used_e lv (ConstBool _) = False
used_e lv (ConstBits _ _) = False
used_e lv (LV lv') = lv == lv'

used_e lv (BinOp o l r) = used_e lv l || used_e lv r
used_e lv (UnOp o e) = used_e lv e
used_e lv (Call _ es) = any (used_e lv) es
used_e lv (Slice b l h) = any (used_e lv) [b,l,h]
used_e lv (Access a i) = used_e lv a || used_e lv i
used_e lv (Copy e) = used_e lv e
used_e lv (Arr _) = error "Local array, I.O.U."

used_s lv ((Assign lv' e):ss) =
    if lv' == lv
        then False
        else used_e lv e || used_s lv ss

used_s lv ((Return e):_) = used_e lv e
used_s lv ((If c t e):ss) =
    used_e lv c || used_b lv t || used_b lv e || used_s lv ss
used_s lv ((For i l h b):ss) =
    (used_e lv l || used_e lv h)
 || ((not $ hidden lv [DeclLocal i Int]) && used_b lv b)
 || used_s lv ss

used_s lv (_:ss) = used_s lv ss
used_s lv [] = False

used_b lv (ds, s) =
    if hidden lv ds
        then False
        else used_s lv (flatten s)

-- Is the variable written before use?
written lv (Assign lv' e) = if lv == lv' then
                                if used_e lv e
                                then error "liveness wat"
                                else True
                            else False
written lv _              = False

data LivSt = LivSt { live :: S.Set LValue }

on  f lv = \l -> if l == lv then True  else f lv
off f lv = \l -> if l == lv then False else f lv

tracked_decl (DeclLocal _ Bits) = True
tracked_decl _ = False

flatten (Seq l r) = flatten l ++ flatten r
flatten s = [s]

sfold = foldl sseq Skip

liv u ls [] = []
liv u ls (s:ss) =
    case s of
        Assign l e ->
            if S.member l ls
            then error "liveness wat 2"
            else let nls = S.fromList $ filter (flip used_s ss) u
                     l_out = if elem l u
                             then S.insert l ls
                             else ls
                     poof = S.difference l_out nls
                     f = map Free (S.toList poof)
                     s' = liv u nls ss
                  in [s] ++ f ++ s'
        _ ->
            let s' = liv u ls ss
             in [s] ++ s'

test = [Assign (LVar "b") (LV (LVar "b'")),
        Assign (Temp 0) (BinOp RRot (LV (LVar "b")) (ConstInt 3)),
        Assign (Temp 1) (BinOp Xor (LV (LVar "b")) (LV (Temp 0)))
       ]
