module Liveness where

import IR
import qualified Data.Set as S
import Debug.Trace
import Common

---------------------------------------------------------------------
-- Live variable analysis
---------------------------------------------------------------------

liveness (d, s) =
    let tracked = track_set d
        s' = sfold $ liv tracked S.empty S.empty $ flatten s
     in (d, s')

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
used_s lv ((Error _):ss) =
    False

used_s lv (_:ss) = used_s lv ss
used_s lv [] = False

used_b lv (ds, s) =
    if hidden lv ds
        then False
        else used_s lv (flatten s)

tracked_decl (DeclLocal _ Bits) = True
tracked_decl _ = False

locals ds = concatMap ff ds where
         ff (DeclLocal lv _) = [lv]
         ff _ = []

track_set :: [Decl] -> S.Set LValue
track_set d = S.fromList $ locals $ filter tracked_decl d

flatten (Seq l r) = flatten l ++ flatten r
flatten s = [s]

liv u ls lo [] = []
liv u ls lo (s:ss) =
    case s of
        Return e ->
            case e of
                IR.LV lv ->
                    let free = map Free (S.toList $ S.delete lv ls)
                     in free ++ [s]
                _ ->
                    error "liveness: return of non-lv"

        Error m ->
            let free = map Free (S.toList ls)
                     in free ++ [s]

        Assign l e | S.member l ls ->
            if used_e l e
                then error "liveness wat 2"
                else Free l : (liv u (S.delete l ls) lo (s:ss))

        -- TODO: replace this "False" with "the variable is
        -- overwritten before any read", which isn't the same as
        -- "not used" since it may belong to lo
        Assign l (Copy (LV lv)) | not (used_s lv ss)
                               && (not (S.member lv lo) || False)
                               && S.member lv u ->
            liv u (S.delete lv ls) lo ((Assign l (LV lv)):ss)

        Assign l e | S.member l u ->
            let l_out = if S.member l u
                        then S.insert l ls
                        else ls
                used = S.filter (flip used_s ss) l_out
                poof = l_out S.\\ (S.union lo used)
                f = map Free (S.toList poof)
                s' = liv u (l_out S.\\ poof) lo ss
             in [s] ++ f ++ s'

        If c t e ->
            let t' = liv_block u ls t
                e' = liv_block u ls e
             in [If c t' e']
                ++ liv u ls lo ss

        For i l h b ->
            let b' = liv_block u ls b
             in [For i l h b']
                ++ liv u ls lo ss

        _ ->
            let s' = liv u ls lo ss
             in [s] ++ s'

liv_block u ls (d, s) =
    let u' = S.union (S.difference u (S.fromList $ locals d)) (track_set d)
        s' = sfold $ liv u' ls ls $ flatten s
     in (d, s')
