module Liveness where

import qualified Data.Set as S
import Data.List
import qualified Data.Map.Strict as M
import Debug.Trace
import Control.Monad.Identity
import Control.Monad.State

import Common
import IR

---------------------------------------------------------------------
-- Live variable analysis
---------------------------------------------------------------------

data LMState = LMState {
    live :: S.Set LValue,
    kind :: M.Map LValue Type,
    live_out :: S.Set LValue,
    univ :: S.Set LValue
 }

type LM = StateT LMState Identity

mod_live f =
    do s <- get
       put (s { live = f (live s) })
mod_kind f =
    do s <- get
       put (s { kind = f (kind s) })
mod_live_out f =
    do s <- get
       put (s { live_out = f (live_out s) })
mod_univ f =
    do s <- get
       put (s { univ = f (univ s) })

add_live :: LValue -> LM ()
add_live (Access _ _) = do return ()
add_live l = mod_live (S.insert l)

add_live_out :: LValue -> LM ()
add_live_out = mod_live_out.(S.insert)

add_univ :: LValue -> Type -> LM ()
add_univ lv t = do mod_univ (S.insert lv)
                   mod_kind (M.insert lv t)

del_live :: LValue -> LM ()
del_live     = mod_live.(S.delete)

del_live_out :: LValue -> LM ()
del_live_out = mod_live_out.(S.delete)

del_univ :: LValue -> LM ()
del_univ lv = do mod_univ (S.delete lv)
                 s <- get
                 put (s { kind = M.delete lv (kind s) })

getS :: (LMState -> a) -> LM a
getS f = do s <- get
            return (f s)

set_live :: S.Set LValue -> LM ()
set_live u = do s <- get
                put ( s { live = u })

set_kind :: M.Map LValue Type -> LM ()
set_kind u = do s <- get
                put ( s { kind = u })

set_live_out :: S.Set LValue -> LM ()
set_live_out u = do s <- get
                    put ( s { live_out = u })

set_univ :: S.Set LValue -> LM ()
set_univ u = do s <- get
                put ( s { univ = u })

type_of :: LValue -> LM Type
type_of lv = do k <- getS kind
                case M.lookup lv k of
                    Nothing -> error "internal liveness error"
                    Just t -> return t

initState = LMState {
    live = S.empty,
    kind = M.empty,
    live_out = S.empty,
    univ = S.empty
 }

liveness b =
    let m = liv_block b S.empty
        i = runStateT m initState
        (b', _) = runIdentity i
     in b'

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
used_e lv (ConstBits _ e) = used_e lv e
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

flatten (Seq l r) = flatten l ++ flatten r
flatten s = [s]

free_one l =
    do t <- type_of l
       let f = case t of
                   Bits -> Free [l]
                   ArrT Bits (Just i) -> FreeArr [l] i
       return f

free set =
    mapM free_one (S.toList set)

needed ss =
    do lo <- getS live_out
       u <- getS univ
       let used     = S.filter (flip used_s   ss) u
           shadowed = S.filter (flip shadow_s ss) u
           needed   = (S.union lo used) S.\\ shadowed

       return needed

unneeded ss =
    do ls <- getS live
       nn <- needed ss
       return $ ls S.\\ nn

do_frees s =
    do un <- unneeded s
       mapM del_live (S.toList un)
       free un

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
       f <- free (S.delete lv ls)
       return $ f ++ [s]

liv' (s@(Error _):ss) =
    do ls <- getS live
       f <- free ls
       return $ f ++ [s]

liv' (s@(Assign l _):ss) | shadow_s l ss =
    liv ss

liv' (s@(Assign (Access a i) _):ss) =
    do ls <- getS live
       if S.member a ls
       then do ss' <- liv_assign s ss
               let f = Free [Access a i]
               return (f:ss')
       else liv_assign s ss

liv' (s@(Assign l _):ss) =
    do u <- getS univ
       if S.member l u
       then liv_assign s ss
       else do ss' <- liv ss
               return (s:ss')

liv' (s@(If c t e):ss) =
    do ls <- getS live
       nn <- needed ss

       t' <- liv_block t nn
       e' <- liv_block e nn

       set_live nn
       ss' <- liv ss
       return ((If c t' e'):ss')

liv' (s@(For i l h b):ss) =
    do ls <- getS live
       b' <- liv_block b ls
       ss' <- liv ss
       return ((For i l h b'):ss')

liv' (s:ss) =
    do ss' <- liv ss
       return (s:ss')

liv_assign s@(Assign l e) ss =
    do ls <- getS live
       un <- unneeded ss
       nn <- needed ss
       u  <- getS univ

       case s of
        Assign l e | S.member l ls ->
            error $ "BUG: Assigning to live value " ++ show (l, e, ls)

        Assign l _ | not (S.member l nn) && S.member l u ->
            liv ss

        -- Avoid copies of temporaries that will be freed on
        -- the next step
        Assign l (Copy lv) | S.member lv un ->
            do del_live lv
               liv_assign (Assign l (LV lv)) ss

        Assign l (Cluster ce as) ->
            do let un' = S.intersection un (S.fromList (map fst as))
                   as' = map (\(lv,_) -> (lv, S.member lv un')) as
               mapM del_live (S.toList un')
               add_live l
               ss' <- liv ss
               return $ [Assign l (Cluster ce as')] ++ ss'

        -- If the expression is an access (forcefully to a bits[])
        -- then don't add l as a live value, since we don't need to
        -- free it afterwards
        Assign l (LV e) | is_access e ->
            do ss' <- liv ss
               return $ [s] ++ ss'

        Assign l (BinOp op (LV b) s) | S.member b un &&
                                       in_placeable op ->
            do del_live b
               liv_assign (Assign l (IPOp op b s)) ss

        Assign l _ ->
            do add_live l
               ss' <- liv ss
               return $ [s] ++ ss'

is_access (Access _ _) = True
is_access _ = False

tracked_type Bits = True
tracked_type (ArrT Bits _) = True
tracked_type _ = False

add_decl (DeclLocal lv t) =
    do del_univ lv
       when (tracked_type t) $ do add_univ lv t

add_decl (DeclGlobal _ _ _) =
    do return ()

liv_block (ds, s) lo' =
    do u  <- getS univ
       k  <- getS kind
       ls <- getS live
       lo <- getS live_out

       mapM add_decl ds

       set_live ls
       set_live_out lo'

       s' <- liv (flatten s)

       set_univ u
       set_live ls
       set_live_out lo
       set_kind k

       return (ds, sfold s')

in_placeable o = elem o [LRot, RRot]
