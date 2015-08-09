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

data VarSt = Shadowed | Used | Unused
    deriving (Eq, Show)

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
used_e lv (Cluster _ es) = any (used_e lv) es

vst_seq Shadowed _ = Shadowed
vst_seq Used _ = Used
vst_seq Unused x = x

vst_par Shadowed Shadowed = Shadowed
vst_par Used _            = Used
vst_par _ Used            = Used
vst_par Unused Unused     = Unused
vst_par _      _          = Unused -- Unsure about this

varst lv ((Assign lv' e):ss) =
    if lv' == lv && not (used_e lv e)
        then Shadowed
        else if used_e lv e
             then Used
             else varst lv ss

varst lv ((Return e):_) =
    if used_e lv e
        then Used
        else Unused

varst lv ((If c t e):ss) =
    if used_e lv c
    then Used
    else let ts = varst_b lv t
             es = varst_b lv e
             l = ts `vst_par` es
          in l `vst_seq` varst lv ss

varst lv ((For i l h b):ss) =
    if used_e lv l || used_e lv h
    then Used
    else if hidden lv [DeclLocal i Int]
         then varst lv ss
         else case varst_b lv b of
                  Shadowed -> varst lv ss -- This is tricky
                  Used -> Used
                  Unused -> varst lv ss

varst lv ((Error _):ss) =
    Unused

varst lv (_:ss) = varst lv ss
varst lv [] = Unused

varst_b lv (ds, s) =
    if hidden lv ds
        then Unused
        else varst lv (flatten s)

used_s lv s   = varst lv s == Used
unused_s lv s = varst lv s == Unused
shadow_s lv s = varst lv s == Shadowed

tracked_decl (DeclLocal _ Bits) = True
tracked_decl _ = False

locals ds = concatMap ff ds where
         ff (DeclLocal lv _) = [lv]
         ff _ = []

track_set :: [Decl] -> S.Set LValue
track_set d = S.fromList $ locals $ filter tracked_decl d

flatten (Seq l r) = flatten l ++ flatten r
flatten s = [s]

do_frees u ls lo s =
    let used     = S.filter (flip used_s s)   ls
        shadowed = S.filter (flip shadow_s s) ls
        needed   = (S.union lo used) S.\\ shadowed
        unneeded = ls S.\\ needed
     in (ls S.\\ unneeded, map Free (S.toList unneeded))

liv u ls lo ss =
    let (ls', f) = do_frees u ls lo ss
        ss' = liv' u ls' lo ss
     in f ++ ss'

liv' u ls lo [] = []
liv' u ls lo (s:ss) =
    case s of
        Return (IR.LV lv) ->
            let free = map Free (S.toList $ S.delete lv ls)
             in free ++ [s]

        Error m ->
            let free = map Free (S.toList ls)
                     in free ++ [s]

        Assign l e | S.member l ls ->
            error "BUG: Assigning to live value"

        Assign l e | shadow_s l ss ->
            liv u ls lo ss

        -- Avoid copies of temporaries that will be freed on
        -- the next step
        Assign l (Copy (LV lv)) | not (used_s lv ss)
                               && (not (S.member lv lo) || shadow_s lv ss)
                               && S.member lv u ->
            liv u (S.delete lv ls) lo ((Assign l (LV lv)):ss)

        Assign l e | S.member l u ->
            [s] ++ liv u (S.insert l ls) lo ss

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
