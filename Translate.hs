module Translate where

import CLang as C
import AST as A
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Data.Map.Strict as M
import Common

type NameEnv = M.Map VarName A.Type

data TransState = TSt { envs :: [NameEnv] }
  deriving Show

-- lists' map, since the Data.Map one
-- might hide it
lmap = Prelude.map

initState = TSt { envs = [] }

getEnvs :: TM [NameEnv]
getEnvs = do s <- get
             return $ envs s

setEnvs :: [NameEnv] -> TM ()
setEnvs es = do s <- get
                put $ s { envs = es }

pushEnv :: TM ()
pushEnv = do e <- getEnvs
             setEnvs (empty : e)

popEnv :: TM ()
popEnv = do e <- getEnvs
            setEnvs (tail e)

addToEnv :: VarName -> A.Type -> TM ()
addToEnv n t = do e:es <- getEnvs
                  setEnvs $ (insert n t e) : es

type TranslateMonad = ErrorT CmtError (
                       StateT TransState (
                        Identity
                      ))

type TM = TranslateMonad -- Only for brevity

add_builtins :: TM ()
add_builtins = do return ()

translate :: A.Prog -> TM C.Prog
translate decls = do pushEnv -- global environment
                     add_builtins -- add builtins to the environment
                     dd <- mapM tr1 decls
                     return $ concat dd

infer :: A.Expr -> TM A.Type
infer (A.ConstStr _)    = do return A.String
infer (A.ConstInt _)    = do return A.Int
infer (A.ConstFloat _)  = do return A.Double
infer (A.TFalse)        = do return A.Bool
infer (A.TTrue)         = do return A.Bool

tmap :: A.Type -> TM C.Type
tmap A.Int      = do return C.Int
tmap A.Bool     = do return C.Bool
tmap A.String   = do return C.String
tmap A.Void     = do return C.Void
tmap A.Double   = do return C.Double
tmap A.Bytes    = do return (C.Ptr C.Void)

tr1 :: A.Decl -> TM [C.Unit]
tr1 (A.VarDecl n Nothing Nothing) =
    -- When there's no type nor expression, assume it's an int.
    -- Later, we'll try to infer the type.
    tr1 (A.VarDecl n (Just A.Int) Nothing)

tr1 (A.VarDecl n (Just typ) Nothing) =
    do tt <- tmap typ
       addToEnv n typ
       return [C.VarDecl n tt []]

tr1 (A.VarDecl n Nothing (Just expr)) =
    do ta <- infer expr
       typ <- tmap ta
       addToEnv n ta
       return [C.VarDecl n typ []]

tr1 (A.VarDecl n (Just ta) (Just expr)) =
    do ta' <- infer expr
       if ta' /= ta
         then throwError CmtErr
         else return ()

       typ <- tmap ta
       addToEnv n ta
       return [C.VarDecl n typ []]

-- it makes no sense to split the logic for externs/consts
-- we should allow multiple modifiers like C. This is to be done
-- when possible
tr1 (A.External name typ) =
    do tc <- tmap typ
       addToEnv name typ
       return [C.VarDecl name tc [C.Extern]]

tr1 (A.Const name expr) =
    do ta <- infer expr
       tc <- tmap ta
       addToEnv name ta
       return [C.VarDecl name tc [C.Const]]

tr1 (A.Struct) =
    do return []

tr1 (A.FunDecl { A.name = n, A.ret = r, A.args = a, A.body = b}) =
    do rc <- tmap r
       let ata = lmap snd a
       atc <- mapM tmap ata
       let argsc = zip (lmap fst a) atc
       addToEnv n (A.Fun ata r)
       pushEnv
       bc <- trstm b
       popEnv
       return [FunDef (Funtype { C.name = n, C.args = argsc,
                                 C.ret = rc }) bc]

trexp :: A.Expr -> TM C.Expr
trexp _ = do return $ C.Var "crap"

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

trstm _ = do return C.Skip

runTranslate :: TM a -> (TransState, Either CmtError a)
runTranslate m = let a = runErrorT m
                     b = runStateT a initState
                     (c, s) = runIdentity b
                  in case c of
                       Left e -> (s, Left e)
                       Right a' -> (s, Right a')

semanticT :: A.Prog -> (TransState, Either CmtError C.Prog)
semanticT = runTranslate.translate
