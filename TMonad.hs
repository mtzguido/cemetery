module TMonad where

import Common
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified AST as A
import qualified IR as I

-- Monad definition


data Attr = RO
  deriving (Eq, Show)

data ExprSeman =
    ExprSeman {
        prep  :: I.Stmt,
        typ   :: A.Type,
        expr  :: I.Expr,
        attrs :: [Attr]
    }
  deriving (Show)

type EnvT = M.Map A.VarName ExprSeman

eseman = ExprSeman {
             prep = I.Skip,
             typ = A.Invalid,
             expr = I.LV $ I.LVar "wat",
             attrs = []
         }

-- State at each level inside the AST
data LevelState =
  LevelState {
      -- env holds the mapping from Cemetery variables
      -- to their types and register where they're stored.
      env :: EnvT,

      -- ret_type is the current function's return type,
      -- to check for invalid returns
      ret_type :: A.Type,

      -- Declarations that should be made at the beginning
      -- of the current block
      decls :: [I.Decl],

      -- Counter for fresh variables
      fresh_count :: Int,

      depth :: Int
  }
  deriving (Show)

blank_level :: LevelState
blank_level = LevelState {
                  env = M.empty,
                  ret_type = A.Invalid,
                  decls = [],
                  fresh_count = 0,
                  depth = 0
              }

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
               -- duplicate the current level, except for the
               -- declarations which are already made
               let l = (head e) { decls = [], depth = depth (head e) + 1 }
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

addDecl :: I.Decl -> TM ()
addDecl d = do l <- getLevel
               setLevel (l { decls = d : decls l })

env_lookup :: String -> TM ExprSeman
env_lookup s = do e <- getEnv
                  case M.lookup s e of
                    Nothing -> abort $ "undefined variable: " ++ s
                    Just i -> return i

addToEnv :: String -> ExprSeman -> TM ()
addToEnv n d = do e <- getEnv
                  let e' = M.insert n d e
                  setEnv e'

fresh :: I.Type -> TM I.LValue
fresh typ =
    do s <- getLevel
       setLevel (s { fresh_count = fresh_count s + 1})
       addDecl (I.DeclLocal (I.Temp $ fresh_count s) typ)
       return $ I.Temp (fresh_count s)

-- Translator Monad definition
type TM =
  StateT TransState (
   ErrorT CmtError (
    Identity
  ))

runTranslate :: TM a -> Either CmtError (a, TransState)
runTranslate m = runIdentity $ runErrorT $ runStateT m initState

abort     s = throwError $ CmtErr s
abortIf b s = when b (abort s)
