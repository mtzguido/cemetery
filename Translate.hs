module Translate where

import CLang as C
import AST as A
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Data.Map.Strict as M
import Common

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

popLevel :: TM ()
popLevel = do e <- getData
              setData (tail e)

addToEnv :: VarName -> (A.Type, VarName, C.Type) -> TM ()
addToEnv n t = do e:es <- getData
                  let (m, d) = e
                  setData $ (insert n t m, d) : es

add_builtins :: TM ()
add_builtins = do return ()

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
infer (A.TFalse)        = do return A.Bool
infer (A.TTrue)         = do return A.Bool

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
       pushLevel
       body <- trbody b
       popLevel
       return [FunDef funt body]

-- Statement and expression translations
-- Abandon all hope ye who enter below this line

trbody :: A.Stmt -> TM C.Block
trbody s = do st <- trstm s
              return ([], st)

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

trexp :: A.Expr -> TM C.Expr
trexp _ = do return $ C.Var "crap"
