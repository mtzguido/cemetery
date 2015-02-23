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
                     dd <- mapM translate1 decls
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

translate1 :: A.Decl -> TM [C.Unit]
translate1 (A.VarDecl n Nothing Nothing) =
    -- When there's no type nor expression, assume it's an int.
    -- Later, we'll try to infer the type.
    translate1 (A.VarDecl n (Just A.Int) Nothing)

translate1 (A.VarDecl n (Just typ) Nothing) =
    do tt <- tmap typ
       addToEnv n typ
       return [C.VarDecl n tt []]

translate1 (A.VarDecl n Nothing (Just expr)) =
    do ta <- infer expr
       typ <- tmap ta
       addToEnv n ta
       return [C.VarDecl n typ []]

translate1 (A.VarDecl n (Just ta) (Just expr)) =
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
translate1 (A.External name typ) =
    do tc <- tmap typ
       addToEnv name typ
       return [C.VarDecl name tc [C.Extern]]

translate1 (A.Const name expr) =
    do ta <- infer expr
       tc <- tmap ta
       addToEnv name ta
       return [C.VarDecl name tc [C.Const]]

translate1 _ =
    do return []

runTranslate :: TM a -> Either CmtError a
runTranslate m = let a = runErrorT m
                     b = runStateT a initState
                     (c, s) = runIdentity b
                  in case c of
                       Left e -> Left e
                       Right a' -> Right a'

semanticT :: A.Prog -> Either CmtError C.Prog
semanticT = runTranslate.translate
