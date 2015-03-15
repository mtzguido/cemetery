module Translate where

import CLang as C
import AST as A

import Common
import Control.Exception
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Data.Map.Strict as M
import Data.Maybe
import Debug.Trace

-- Monad definition

data LevelState = LevelState {
                      env :: M.Map VarName (A.Type, VarName, C.Type),
                      opening :: [C.Decl],
                      ret_type :: A.Type
                  }
                  deriving (Show)

blank_level :: LevelState
blank_level = LevelState { env = empty, opening = [], ret_type = A.Invalid }

data TransState = TSt { level_data :: [LevelState] }
  deriving Show

initState = TSt { level_data = [] }

type TranslateMonad = ErrorT CmtError (
                       StateT TransState (
                        Identity
                      ))

type TM = TranslateMonad -- Only for brevity

runTranslate :: TM a -> (TransState, Either CmtError a)
runTranslate m = let a = runErrorT m
                     b = runStateT a initState
                     (c, s) = runIdentity b
                  in case c of
                       Left e -> (s, Left e)
                       Right a' -> (s, Right a')

semanticT :: A.Prog -> (TransState, Either CmtError C.Prog)
semanticT = runTranslate.translate

-- Good ol' list map, since the Data.Map one might hide it
lmap = Prelude.map

-- Environment handling functions

getData :: TM [LevelState]
getData = do s <- get
             return $ level_data s

setData :: [LevelState] -> TM ()
setData es = do s <- get
                put $ s { level_data = es }

getRetType :: TM A.Type
getRetType = do s <- get
                let d = level_data s
                return $ ret_type (head d)

setRetType :: A.Type -> TM ()
setRetType t = do s <- get
                  let h:ts = level_data s
                  put $ s { level_data = h { ret_type = t } : ts }

pushLevel :: TM ()
pushLevel = do e <- getData
               setData (blank_level : e)

popLevel :: TM LevelState
popLevel = do e <- getData
              setData (tail e)
              return (head e)

addToEnv :: VarName -> (A.Type, VarName, C.Type) -> TM ()
addToEnv n t = do e:es <- getData
                  setData $ (e { env = insert n t (env e) }) : es

addDecl :: C.Decl -> TM ()
addDecl d = do l:ls <- getData
               setData $ l { opening = d : opening l } : ls

add_builtins :: TM ()
add_builtins = do addToEnv "trunc" (A.Fun [A.Bytes] A.Bytes, "__cmt_trunc", C.Int)
                  addToEnv "repeat" (A.Fun [A.Bytes] A.Bytes, "__cmt_repeat", C.Int)
                  return ()

ff :: Maybe a -> Maybe a -> Maybe a
ff (Just x) _ = Just x
ff Nothing m = m

env_lookup :: String -> TM (A.Type, VarName, C.Type)
env_lookup s = do d <- getData
                  let dd = lmap env d
                  let f = Prelude.foldl (\a v -> ff a (M.lookup s v)) Nothing dd
                  case f of
                    Nothing -> error $ "undefined variable: " ++ s
                    Just i -> return i

-- Main translation procedure

translate :: A.Prog -> TM C.Prog
translate decls = do pushLevel -- global environment
                     add_builtins -- add builtins to the environment
                     dd <- mapM tr1 decls
                     return $ concat dd

-- Type inference and mapping

infer :: A.Expr -> TM A.Type
infer (A.ConstStr _)    = do return A.String
infer (A.ConstInt _)    = do return A.Int
infer (A.ConstFloat _)  = do return A.Double
infer (A.ConstBool _)   = do return A.Bool
infer (A.BinLit _)      = do return A.Bytes
infer e = do trace ("e = " ++ show e) (error "")

tmap :: A.Type -> TM C.Type
tmap A.Int      = do return C.Int
tmap A.Bool     = do return C.Bool
tmap A.String   = do return C.String
tmap A.Void     = do return C.Void
tmap A.Double   = do return C.Double
tmap A.Bytes    = do return (C.Ptr C.Void)

-- Unit translations

tr1 :: A.Decl -> TM [C.Unit]
tr1 (A.VarDecl mods n Nothing Nothing) =
    -- When there's no type nor expression, assume it's an int.
    -- Later, we'll try to infer the type.
    tr1 (A.VarDecl mods n (Just A.Int) Nothing)

tr1 (A.VarDecl n mods (Just typ) Nothing) =
    do tt <- tmap typ
       addToEnv n (typ, n, tt)
       return [C.Decl $ C.VarDecl n tt []]

tr1 (A.VarDecl n mods Nothing (Just expr)) =
    do ta <- infer expr
       typ <- tmap ta
       addToEnv n (ta, n, typ)
       return [C.Decl $ C.VarDecl n typ []]

tr1 (A.VarDecl n mods (Just ta) (Just expr)) =
    do ta' <- infer expr
       if ta' /= ta
         then throwError CmtErr
         else return ()

       typ <- tmap ta
       addToEnv n (ta, n, typ)
       return [C.Decl $ C.VarDecl n typ []]

tr1 (A.Struct) =
    do return []

tr1 (A.FunDecl { A.name = n, A.ret = r, A.args = a, A.body = b}) =
    do rc <- tmap r
       let ata = lmap snd a
       atc <- mapM tmap ata
       let argsc = zip (lmap fst a) atc
       let funt = Funtype { C.name = n, C.args = argsc, C.ret = rc }
       addToEnv n (A.Fun ata r, n, C.Fun funt)

       let add1 ((n, ta), tc) = do addToEnv n (ta, n, tc)

       mapM add1 (zip a atc)

       body <- trbody b r
       return [FunDef funt body]

-- Statement and expression translations
-- Abandon all hope ye who enter below this line

trbody :: A.Stmt -> A.Type -> TM C.Block
trbody s t =
    do pushLevel
       setRetType t
       st <- trstm s
       lvl <- popLevel
       let ds = opening lvl
       trace ("ds = " ++ show ds) (return ())
       return (reverse ds, st)

fromCDecl :: C.Unit -> C.Decl
fromCDecl (C.Decl d) = d
fromCDecl _ = assert False (error "")

trstm :: A.Stmt -> TM C.Stmt
trstm A.Skip = do return C.Skip

trstm (A.Assign n e) =
    do (ee, te) <- trexp e
       (ta, nn, __) <- env_lookup n

       if tmatch ta te
         then return (C.Assign nn ee)
         else error "type mismatch (1)"

trstm (A.Seq l r) =
    do ll <- trstm l
       rr <- trstm r
       return $ C.sseq ll rr

trstm (A.Return e) =
    do (ee, te) <- trexp e
       rt <- getRetType
       if tmatch te rt
         then return (C.Return ee)
         else error "type mismatch (2)"

trstm (A.Decl d) =
    do cds <- tr1 d
       mapM addDecl (lmap fromCDecl cds)
       return C.Skip

trstm (A.If c t e) =
    do (cc, tc) <- trexp c
       tr <- getRetType
       trace ("lala:" ++ show c ++ "::" ++ show cc ++ "::" ++ show tc) (return ())
       if tmatch tc A.Bool || tmatch tc A.Int
         then do tt <- trbody t tr
                 ee <- trbody e tr
                 return (C.If cc tt ee)
         else error "type mismatch (3)"

trexp :: A.Expr -> TM (C.Expr, A.Type)
trexp (A.ConstInt n) =
    do return (C.ConstInt n, A.Int)

trexp (A.ConstFloat f) =
    do return (C.ConstFloat f, A.Double)

trexp (A.ConstBool b) =
    do return (C.ConstBool b, A.Bool)

trexp (A.ConstStr s) =
    do return (C.ConstStr s, A.String)

trexp (A.BinOp a_op l r) =
    do (ll, tl) <- trexp l
       (rr, tr) <- trexp r
       trbinop a_op ll tl rr tr

trexp (A.UnOp a_op e) =
    do (ee, te) <- trexp e
       trunop a_op ee te

trexp (A.Call f args) =
    do (tf, ff, _) <- env_lookup f
       args_ir <- mapM trexp args
       let (args, args_t) = unzip args_ir
       trace ("looking for " ++ f) (return ())
       let A.Fun _ rt = tf
       -- check match for f(a1, a2, ...)
       return (C.Call ff args, rt)

trexp (A.Var v) =
    do (tv, vv, _) <- env_lookup v
       return (C.Var v, tv)

trexp (A.BinLit _) = -- need to add a declaration and point to it
    do return (C.ConstInt 0, A.Bytes)

-- Operator translation

trbinop A.Plus  = arith_op C.Plus
trbinop A.Minus = arith_op C.Minus
trbinop A.Prod  = arith_op C.Prod
trbinop A.Div   = arith_op C.Div
trbinop A.Mod   = arith_op C.Mod
trbinop A.Xor   = tr_xor
trbinop A.Eq    = tr_comparison

arith_op op l lt r rt =
    do if (tmatch lt A.Int && tmatch rt A.Int)
          || (tmatch lt A.Double && tmatch rt A.Double)
           then return (C.BinOp op l r, lt)
           else error "type mismatch (4)"

tr_xor l lt r rt =       -- built-in operator
    do if tmatch lt A.Bytes && tmatch rt A.Bytes
         then return (C.Call "__cmt_xor" [l, r], lt)
         else error "type mismatch (5)"

tr_comparison l lt r rt =
    do if tmatch lt rt -- obviously wrong once we have more rich types
           then return (C.BinOp C.Eq l r, A.Bool)
           else error "type mismatch (6)"

trunop A.NegateNum  = tr_negatenum

tr_negatenum ee te =
    do if tmatch te A.Int
           then return (C.UnOp C.NegateNum ee, te)
           else error "type mismatch (7)"

tmatch :: A.Type -> A.Type -> Bool
tmatch p q = p == q
