-- Cemetery translation module
--
-- This module takes care of translating the Cemetery AST just after
-- parsing into an intermediate representation for it to be later
-- translated into the output language.
--
-- Afterwards, this should go into the Output module (unexistant as of
-- now) which should generate the end result (C Code)
--
-- Right now, this basically fill everything with stubs.

module Translate where

import qualified AST as A
import qualified IR as IR

import Common
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map.Strict as M

-- Monad definition

type EnvT = M.Map A.VarName (A.Type, IR.Reg)

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

getEnv :: TM EnvT
getEnv = do d <- getData
            return (env $ head d)

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
               -- duplicate the current level
               setData ((head e) : e)

popLevel :: TM LevelState
popLevel = do e <- getData
              setData (tail e)
              return (head e)

ff :: Maybe a -> Maybe a -> Maybe a
ff (Just x) _ = Just x
ff Nothing m = m

env_lookup :: String -> TM (A.Type, IR.Reg)
env_lookup s = do e <- getEnv
                  case M.lookup s e of
                    Nothing -> error $ "undefined variable: " ++ s
                    Just i -> return i

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

semanticT :: A.Prog -> (TransState, Either CmtError IR.IR)
semanticT = runTranslate.translate

add_builtins = do return ()

-- Each Cemetery unit is a IR unit, at least for now,
-- so just mapM the unit translation
translate :: A.Prog -> TM IR.IR
translate prog = do -- Push a first level and add builtins to it
                    setData [blank_level]
                    add_builtins
                    ir <- mapM translate1 prog
                    lvl <- popLevel -- Useful?
                    return ir

translate1 :: A.Decl -> TM IR.Unit
translate1 (A.VarDecl _ _ _ _) =
    do return IR.UnitScaf
translate1 (A.Struct) =
    do return IR.UnitScaf
translate1 (A.FunDecl {A.name = name, A.ret = ret,
                       A.args = args, A.body = body}) =
    do pushLevel
       ir_name <- tr_fun_name name
       ir_args <- mapM tr_arg args
       ir_body <- tr_body body
       ir_ret  <- tmap ret
       popLevel
       return $ IR.FunDef (IR.Funtype { IR.name = ir_name,
                                        IR.args = ir_args,
                                        IR.ret  = ir_ret }) ir_body

tr_body :: A.Stmt -> TM IR.Stmt
tr_body _ = do return IR.StmtScaf

tr_arg :: (String, A.Type) -> TM (String, IR.Type)
tr_arg (s, t) = do ir_t <- tmap t
                   return (s, ir_t)

tr_fun_name :: String -> TM String
tr_fun_name s = do return s

tmap :: A.Type -> TM IR.Type
tmap A.Int = do return IR.Int
tmap t = error $ "Can't map that type (" ++ (show t) ++ ")"
