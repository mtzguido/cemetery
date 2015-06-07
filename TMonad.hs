module TMonad where

import Common
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified AST as A

-- Monad definition

type EnvT = M.Map A.VarName EnvV
data EnvV = EnvV { typ :: A.Type, 
                   ir_name :: String
                 }
  deriving (Show)

-- State at each level inside the AST
data LevelState =
  LevelState {
      -- env holds the mapping from Cemetery variables
      -- to their types and register where they're stored.
      env :: EnvT,

      -- ret_type is the current function's return type,
      -- to check for invalid returns
      ret_type :: A.Type
  }
  deriving (Show)

blank_level :: LevelState
blank_level = LevelState { env = M.empty, ret_type = A.Invalid }

-- The monad state is a stack of LevelStates so we can
-- drop the names when moving out of a function.
data TransState = TSt { level_data :: [LevelState] }
  deriving Show

initState = TSt { level_data = [] }

-- Environment handling

getData :: TM [LevelState]
getData = do s <- get
             return $ level_data s

setData :: [LevelState] -> TM ()
setData es = do s <- get
                put $ s { level_data = es }

pushLevel :: TM ()
pushLevel = do e <- getData
               -- duplicate the current level
               let l = (head e)
               setData (l : e)

popLevel :: TM LevelState
popLevel = do e <- getData
              setData (tail e)
              return (head e)

getLevel :: TM LevelState
getLevel = do d <- getData
              return (head d)

setLevel :: LevelState -> TM ()
setLevel l = do d <- getData
                setData (l : tail d)

getEnv :: TM EnvT
getEnv = do l <- getLevel
            return (env l)

setEnv :: EnvT -> TM ()
setEnv e = do l <- getLevel
              setLevel (l { env = e })

getRetType :: TM A.Type
getRetType = do l <- getLevel
                return (ret_type l)

setRetType :: A.Type -> TM ()
setRetType t = do l <- getLevel
                  setLevel (l { ret_type = t})

env_lookup :: String -> TM EnvV
env_lookup s = do e <- getEnv
                  case M.lookup s e of
                    Nothing -> error $ "undefined variable: " ++ s
                    Just i -> return i

addToEnv :: String -> EnvV -> TM ()
addToEnv n d = do e <- getEnv
                  let e' = M.insert n d e
                  setEnv e'

-- Translator Monad definition
type TM =
  ErrorT CmtError (
   StateT TransState (
    Identity
  ))

runTranslate :: TM a -> (TransState, Either CmtError a)
runTranslate m = let a = runErrorT m
                     b = runStateT a initState
                     (c, s) = runIdentity b
                  in case c of
                       Left e -> (s, Left e)
                       Right a' -> (s, Right a')
