module Liveness where

import IR
import qualified Data.Set as S
import Debug.Trace
import Common
import Control.Monad.Identity
import Control.Monad.State

---------------------------------------------------------------------
-- Live variable analysis
---------------------------------------------------------------------

data LMState = LMState {
    live :: S.Set LValue,
    live_out :: S.Set LValue,
    univ :: S.Set LValue
 }

type LM = StateT LMState Identity

mod_live f v =
    do s <- get
       put (s { live = f v (live s) })
mod_live_out f v =
    do s <- get
       put (s { live_out = f v (live_out s) })
mod_univ f v =
    do s <- get
       put (s { univ = f v (univ s) })

add_live :: LValue -> LM ()
add_live     = mod_live     S.insert

add_live_out :: LValue -> LM ()
add_live_out = mod_live_out S.insert

del_live :: LValue -> LM ()
del_live     = mod_live     S.delete

del_live_out :: LValue -> LM ()
del_live_out = mod_live_out S.delete

getS :: (LMState -> a) -> LM a
getS f = do s <- get
            return (f s)

set_live :: S.Set LValue -> LM ()
set_live u = do s <- get
                put ( s { live = u })

set_live_out :: S.Set LValue -> LM ()
set_live_out u = do s <- get
                    put ( s { live_out = u })

set_univ :: S.Set LValue -> LM ()
set_univ u = do s <- get
                put ( s { univ = u })

initState = LMState { live = S.empty, live_out = S.empty, univ = S.empty }

liveness (d, s) =
    let m = do_liv (d, s)
        i = runStateT m initState
        (v, _) = runIdentity i
     in v

do_liv :: ([Decl], Stmt) -> LM ([Decl], Stmt)
do_liv (d, s) =
    do let tracked = track_set d
       set_univ tracked
       s' <- liv (flatten s)
       return (d, sfold s')

track_set :: [Decl] -> S.Set LValue
track_set d = S.fromList $ locals $ filter tracked_decl d

-- Does any of the declarations in ds hide lv?
hidden lv ds =
    [] /= filter (\d -> case d of DeclLocal lv' _ -> lv == lv'
                                  _ -> False) ds

data VarSt = Shadowed | Used | Unused
    deriving (Eq, Show)

-- Is the variable lv read in this chunk, before being written?
used_lv :: LValue -> LValue -> Bool
used_lv lv (Access a i) =
    used_lv lv a || used_e lv i
used_lv lv l =
    lv == l

used_e :: LValue -> Expr -> Bool
used_e lv (ConstInt _) = False
used_e lv (ConstBool _) = False
used_e lv (ConstBits _ _) = False
used_e lv (LV lv') = used_lv lv lv'

used_e lv (BinOp o l r) =
    used_e lv l || used_e lv r
used_e lv (UnOp o e) =
    used_e lv e
used_e lv (Call _ es) =
    any (used_e lv) es
used_e lv (Slice b l h) =
    used_lv lv b || any (used_e lv) [l,h]
used_e lv (Copy lv') =
    used_lv lv lv'
used_e lv (Arr es) =
    any (used_e lv) es
used_e lv (Cluster _ es) =
    any (== lv) (map fst es)

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

flatten (Seq l r) = flatten l ++ flatten r
flatten s = [s]

free set = Free (S.toList set)

unneeded ls lo ss =
    let used     = S.filter (flip used_s   ss) ls
        shadowed = S.filter (flip shadow_s ss) ls
        needed   = (S.union lo used) S.\\ shadowed
     in ls S.\\ needed

do_frees s =
    do ls <- getS live
       lo <- getS live_out
       let un = unneeded ls lo s
       mapM del_live (S.toList un)
       return [free un]

liv ss =
    do f <- do_frees ss
       ss' <- liv' ss
       return (f ++ ss')

liv' [] =
    do return []

-- We need to do frees manually in these two cases since we don't care
-- about keeping the values in "lo", like we usually do
liv' (s@(Return (LV lv)):ss) =
    do ls <- getS live
       return $ [free (S.delete lv ls), s]

liv' (s@(Error _):ss) =
    do ls <- getS live
       return [free ls, s]

liv' (s@(Assign l _):ss) =
    do u <- getS univ
       if shadow_s l ss
       then liv ss
       else if S.member l u
            then liv_assign s ss
            else do ss' <- liv ss
                    return (s:ss')

liv' (s@(If c t e):ss) =
    do t' <- liv_block t
       e' <- liv_block e
       ss' <- liv ss
       return ((If c t' e'):ss')

liv' (s@(For i l h b):ss) =
    do b' <- liv_block b
       ss' <- liv ss
       return ((For i l h b'):ss')

liv' (s:ss) =
    do ss' <- liv ss
       return (s:ss')

liv_assign s@(Assign l e) ss =
    do ls <- getS live
       lo <- getS live_out
       u <- getS univ
       case s of
        Assign l e | S.member l ls ->
            error $ "BUG: Assigning to live value " ++ show (l, e, ls)

        Assign l e | shadow_s l ss ->
            liv ss

        -- Avoid copies of temporaries that will be freed on
        -- the next step
        Assign l (Copy lv) | not (used_s lv ss)
                          && (not (S.member lv lo) || shadow_s lv ss)
                          && S.member lv u ->
            do del_live lv
               liv ((Assign l (LV lv)):ss)

        Assign l (Cluster ce as) ->
            do let un  = unneeded ls lo ss
                   un' = S.intersection un (S.fromList (map fst as))
                   as' = map (\(lv,_) -> (lv, S.member lv un')) as
               mapM del_live (S.toList un')
               add_live l
               ss' <- liv ss
               return $ [Assign l (Cluster ce as')] ++ ss'

        Assign l e ->
            do add_live l
               ss' <- liv ss
               return $ [s] ++ ss'

liv_block (d, s) =
    do u <- getS univ
       ls <- getS live
       lo <- getS live_out

       let u' = S.union (S.difference u (S.fromList $ locals d)) (track_set d)

       set_univ u'
       set_live ls
       set_live_out ls

       s' <- liv (flatten s)

       set_univ u
       set_live ls
       set_live_out lo

       return (d, sfold s')
