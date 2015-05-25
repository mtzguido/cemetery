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
      ret_type :: A.Type,

      -- Counter to create new fresh temporary registers,
      -- in a very simple and crappy way
      fresh_counter :: Int
  }
  deriving (Show)

blank_level :: LevelState
blank_level = LevelState { env = M.empty, ret_type = A.Invalid,
                           fresh_counter = 0 }

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
               setData (head e : e)

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

ff :: Maybe a -> Maybe a -> Maybe a
ff (Just x) _ = Just x
ff Nothing m = m

env_lookup :: String -> TM (A.Type, IR.Reg)
env_lookup s = do e <- getEnv
                  case M.lookup s e of
                    Nothing -> error $ "undefined variable: " ++ s
                    Just i -> return i

addToEnv :: String -> A.Type -> IR.Reg -> TM ()
addToEnv n t r = do e <- getEnv
                    let e' = M.insert n (t, r) e
                    setEnv e'

fresh :: TM IR.Reg
fresh = do l <- getLevel
           let i = fresh_counter l
           setLevel (l { fresh_counter = i + 1 })
           return $ IR.Temp i

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

-- IR Helpers

sseq :: IR.Stmt -> IR.Stmt -> IR.Stmt
sseq (IR.Skip) r = r
sseq l (IR.Skip) = l
sseq l r = IR.Seq l r

irlist :: [IR.Stmt] -> IR.Stmt
irlist l = foldl sseq IR.Skip l

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
       ir_ret  <- tmap ret
       ir_body <- tr_stmt body
       popLevel
       return $ IR.FunDef (IR.Funtype { IR.name = ir_name,
                                        IR.args = ir_args,
                                        IR.ret  = ir_ret }) ir_body

tr_stmt :: A.Stmt -> TM IR.Stmt
tr_stmt A.Skip =
    do return IR.Skip

tr_stmt (A.Decl (A.VarDecl name mods mt me)) =
    do --ir_name <- getUnusedName name
       ir_reg <- fresh
       addToEnv name A.Int ir_reg
       return IR.Skip

tr_stmt (A.Seq l r) =
    do l_ir <- tr_stmt l
       r_ir <- tr_stmt r
       return (sseq l_ir r_ir)

tr_stmt (A.Assign name e) =
    do (t, rv) <- env_lookup name
       (e_ir, e_res) <- tr_expr e
       return $ sseq e_ir (IR.Assign rv e_res)

tr_stmt (A.Return e) =
    do (e_ir, e_res) <- tr_expr e
       return $ sseq e_ir (IR.Return e_res)

tr_stmt (A.If c t e) =
    do (c_ir, c_reg) <- tr_expr c
       pushLevel
       t_ir <- tr_stmt t
       popLevel
       pushLevel
       e_ir <- tr_stmt e
       popLevel
       return $ sseq c_ir (IR.If c_reg t_ir e_ir)

tr_stmt _ =
    do return IR.StmtScaf

tr_expr :: A.Expr -> TM (IR.Stmt, IR.Reg)
tr_expr (A.ConstInt i) =
    do r <- fresh
       return (IR.AssignInt r i, r)

tr_expr (A.BinOp A.Plus l r)  = simple_binop_translate IR.Plus  l r
tr_expr (A.BinOp A.Minus l r) = simple_binop_translate IR.Minus l r
tr_expr (A.BinOp A.Div l r)   = simple_binop_translate IR.Div   l r
tr_expr (A.BinOp A.Prod l r)  = simple_binop_translate IR.Prod  l r

tr_expr (A.Var name) =
    do (t, r) <- env_lookup name
       return (IR.Skip, r)

tr_expr _ =
    do return (IR.StmtScaf, IR.regn 999)

tr_arg :: (String, A.Type) -> TM (String, IR.Type)
tr_arg (s, t) = do ir_t <- tmap t
                   ir_reg <- fresh
                   addToEnv s t ir_reg
                   return (s, ir_t)

tr_fun_name :: String -> TM String
tr_fun_name s = do return s

tmap :: A.Type -> TM IR.Type
tmap A.Int = do return IR.Int
tmap t = error $ "Can't map that type (" ++ (show t) ++ ")"

simple_binop_translate :: IR.BinOp -> A.Expr -> A.Expr -> TM (IR.Stmt, IR.Reg)
simple_binop_translate ir_op l r =
    do (l_ir, l_reg) <- tr_expr l
       (r_ir, r_reg) <- tr_expr r
       e_reg <- fresh
       return (irlist [l_ir, r_ir, IR.AssignOp ir_op e_reg l_reg r_reg],
               e_reg)
