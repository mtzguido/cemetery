module Translate where

import CLang as C
import AST as A
import Control.Monad.Identity
import Control.Monad.State
import Data.Map.Strict as M

type NameEnv = M.Map VarName A.Type

data TransState = TSt { envs :: [NameEnv] }

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

type TranslateMonad = StateT TransState Identity
type TM = TranslateMonad -- Only for brevity

add_builtins :: TM ()
add_builtins = do return ()

translate :: A.Prog -> TM C.Prog
translate decls = do pushEnv -- global environment
                     add_builtins -- add builtins to the environment
                     dd <- mapM translate1 decls
                     return $ concat dd

translate1 :: A.Decl -> TM [C.Unit]
translate1 (A.VarDecl n t e) = do return [C.VarDecl n C.Int []]

translate1 d = do return [UnitStub]

runTranslate :: TM a -> a
runTranslate m = let a = runStateT m initState
                     (a', s) = runIdentity a
                  in a'

semanticT :: A.Prog -> C.Prog
semanticT = runTranslate.translate
