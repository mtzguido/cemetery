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

type LevelState = (M.Map VarName (A.Type, VarName, C.Type), [C.Decl])

blank_level :: LevelState
blank_level = (empty, [])

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

pushLevel :: TM ()
pushLevel = do e <- getData
               setData (blank_level : e)

popLevel :: TM LevelState
popLevel = do e <- getData
              setData (tail e)
              return (head e)

addToEnv :: VarName -> (A.Type, VarName, C.Type) -> TM ()
addToEnv n t = do e:es <- getData
                  let (m, d) = e
                  setData $ (insert n t m, d) : es

addDecl :: C.Decl -> TM ()
addDecl d = do l:ls <- getData
               let l' = (\(a,b) -> (a, d:b)) l
               setData (l':ls)

add_builtins :: TM ()
add_builtins = do addToEnv "trunc" (A.Int, "__cmt_trunc", C.Int)
                  addToEnv "repeat" (A.Int, "__cmt_repeat", C.Int)
                  return ()

ff :: Maybe a -> Maybe a -> Maybe a
ff (Just x) _ = Just x
ff Nothing m = m

env_lookup :: String -> TM (A.Type, VarName, C.Type)
env_lookup s = do d <- getData
                  let dd = lmap fst d
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
       body <- trbody b
       return [FunDef funt body]

-- Statement and expression translations
-- Abandon all hope ye who enter below this line

trbody :: A.Stmt -> TM C.Block
trbody s = do pushLevel
              st <- trstm s
              (env, ds) <- popLevel
              trace ("ds = " ++ show ds) (return ())
              return (reverse ds, st)

fromCDecl :: C.Unit -> C.Decl
fromCDecl (C.Decl d) = d
fromCDecl _ = assert False (error "")

trstm :: A.Stmt -> TM C.Stmt
trstm A.Skip = do return C.Skip

trstm (A.Assign n e) =
    do ee <- trexp e
       return $ C.Assign n ee

trstm (A.Seq l r) =
    do ll <- trstm l
       rr <- trstm r
       return $ C.sseq ll rr

trstm (A.Return e) =
    do ee <- trexp e
       return $ C.Return ee

trstm (A.Decl d) =
    do cds <- tr1 d
       mapM addDecl (lmap fromCDecl cds)
       return C.Skip

trstm (A.If c t e) =
    do cc <- trexp c
       tt <- trbody t
       ee <- trbody e
       return $ C.If cc tt ee

trexp :: A.Expr -> TM C.Expr
trexp (A.ConstInt n) =
    do return (C.ConstInt n)

trexp (A.ConstFloat f) =
    do return (C.ConstFloat f)

trexp (A.ConstBool b) =
    do return (C.ConstBool b)

trexp (A.BinOp Xor l r) =       -- built-in operator
    do ll <- trexp l
       rr <- trexp r
       return (C.Call "__cmt_xor" [ll, rr])

trexp (A.BinOp a_op l r) =
    do c_op <- trbinop a_op
       ll <- trexp l
       rr <- trexp r
       return (C.BinOp c_op ll rr)

trexp (A.UnOp a_op e) =
    do c_op <- trunop a_op
       ee <- trexp e
       return (C.UnOp c_op ee)

trexp (A.Call f args) =
    do (_, ff, _) <- env_lookup f
       ee <- mapM trexp args
       return $ C.Call ff ee

trexp _ = do return $ C.Var "crap"

trbinop A.Plus      = do return C.Plus
trbinop A.Minus     = do return C.Minus
trbinop A.Div       = do return C.Div
trbinop A.Prod      = do return C.Prod
trbinop A.Eq        = do return C.Eq
trbinop A.Mod       = do return C.Mod

trunop A.NegateNum  = do return C.NegateNum
