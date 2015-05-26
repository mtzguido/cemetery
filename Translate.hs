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
import Builtins

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
      fresh_counter :: Int,

      temp_decls :: [(IR.Reg, IR.Type)]
  }
  deriving (Show)

blank_level :: LevelState
blank_level = LevelState { env = M.empty, ret_type = A.Invalid,
                           fresh_counter = 0, temp_decls = [] }

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
               -- duplicate the current level, except for temp_decls
               -- which shouldn't empty as its data is not cumulative
               let l = (head e) { temp_decls = [] }
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

addRegDecl :: IR.Reg -> IR.Type -> TM ()
addRegDecl r t = do l <- getLevel
                    let ds = temp_decls l
                    setLevel l { temp_decls = (r,t) : ds }

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

env_lookup :: String -> TM (A.Type, IR.Reg )
env_lookup s = do e <- getEnv
                  case M.lookup s e of
                    Nothing -> error $ "undefined variable: " ++ s
                    Just i -> return i

addToEnv :: String -> A.Type -> IR.Reg -> TM ()
addToEnv n t r = do e <- getEnv
                    let e' = M.insert n (t, r) e
                    setEnv e'

fresh :: IR.Type -> TM IR.Reg
fresh t = do l <- getLevel
             let i = fresh_counter l
             setLevel (l { fresh_counter = i + 1 })
             let r = IR.Temp i
             addRegDecl r t
             return r

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

add_one (name, typ, c_name) =
    do addToEnv name typ (IR.Lit c_name)

add_builtins =
    do mapM add_one builtins

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
    do -- First translate the name, and add it to the
       -- environment. This enables recursion.
       ir_name <- getUnusedName name
       let fun_t = A.Fun (map snd args) ret
       addToEnv name fun_t (IR.Lit ir_name)

       pushLevel
       ir_args <- mapM tr_arg args
       ir_ret  <- tmap ret
       ir_body <- tr_stmt body
       lvl <- popLevel

       full_body <- addRegDecls lvl ir_body

       return $ IR.FunDef (IR.Funtype { IR.name = ir_name,
                                        IR.args = ir_args,
                                        IR.ret  = ir_ret }) full_body

addRegDecls :: LevelState -> IR.Stmt -> TM IR.Stmt
addRegDecls lvl body =
    do let ds = map (\(r,t) -> IR.RegDecl r t) $ reverse $ temp_decls lvl
       let ds_ir = foldl sseq IR.Skip ds
       return (sseq ds_ir body)

getUnusedName n = do return n

tr_stmt :: A.Stmt -> TM IR.Stmt
tr_stmt A.Skip =
    do return IR.Skip

tr_stmt (A.Decl (A.VarDecl name mods mt me)) =
    do when (mt == Nothing && me == Nothing) $
         error "Variables need to have either a type or an initializer"
       ir_name <- getUnusedName name
       ir_t <- tmap A.Int               -- FIXME
       addToEnv name A.Int (IR.Lit ir_name)
       addRegDecl (IR.Lit ir_name) ir_t
       return IR.Skip

tr_stmt (A.Seq l r) =
    do l_ir <- tr_stmt l
       r_ir <- tr_stmt r
       return (sseq l_ir r_ir)

tr_stmt (A.Assign name e) =
    do (t, rv) <- env_lookup name
       (_, e_ir, e_res) <- tr_expr e
       return $ sseq e_ir (IR.Assign rv e_res)

tr_stmt (A.Return e) =
    do (_, e_ir, e_res) <- tr_expr e
       return $ sseq e_ir (IR.Return e_res)

tr_stmt (A.If c t e) =
    do (_, c_ir, c_reg) <- tr_expr c
       pushLevel
       t_body <- tr_stmt t
       lvl_t <- popLevel
       pushLevel
       e_body <- tr_stmt e
       lvl_e <- popLevel

       t_ir <- addRegDecls lvl_t t_body
       e_ir <- addRegDecls lvl_e e_body
       return $ sseq c_ir (IR.If c_reg t_ir e_ir)

tr_expr :: A.Expr -> TM (A.Type, IR.Stmt, IR.Reg)
tr_expr (A.ConstInt i) =
    do r <- fresh IR.Int
       return (A.Int, IR.AssignInt r i, r)

tr_expr (A.BinOp op l r) =
    do (l_typ, l_ir, l_reg) <- tr_expr l
       (r_typ, r_ir, r_reg) <- tr_expr r

       possible <- find_matching_binop op l_typ r_typ
       let (typ, ir_op) = case possible of
                            [] -> error "Operator type mismatch"
                            [(t, o)] -> (t, o)
                            _ -> error "What"

       ir_t <- tmap typ
       e_reg <- fresh ir_t

       let stmt = irlist [l_ir, r_ir, IR.AssignBinOp ir_op e_reg l_reg r_reg]
       return (typ, stmt, e_reg)

tr_expr (A.UnOp op e) =
    do (e_typ, e_ir, e_reg) <- tr_expr e

       possible <- find_matching_unop op e_typ
       let (typ, ir_op) = case possible of
                            [] -> error "Operator type mismatch"
                            [(t, o)] -> (t, o)
                            _ -> error "What"

       ir_t <- tmap typ
       r_reg <- fresh ir_t

       let stmt = irlist [e_ir, IR.AssignUnOp ir_op r_reg e_reg]
       return (typ, stmt, e_reg)

tr_expr (A.Var name) =
    do (t, r) <- env_lookup name
       return (t, IR.Skip, r)

tr_expr (A.Call name args) =
    do args_tr <- mapM tr_expr args
       (f_type, f_sym) <- env_lookup name
       let A.Fun f_args_t ret_type = f_type

       let ir_name = case f_sym of
                       IR.Temp _ -> error "symbol is not a cemtery func"
                       IR.Lit n -> n

       let (args_t, args_ir, args_regs) = unzip3 args_tr

       let ok = all id $ map (uncurry tmatch) (zip f_args_t args_t)
       when (not ok) $ error "call arguments do not type"

       ir_ret_t <- tmap ret_type
       result <- fresh ir_ret_t
       let call = IR.Call ir_name args_regs result
       return (f_type, irlist (args_ir ++ [call]), result)

tr_expr _ = -- FIXME
    do r <- fresh IR.Int
       return (A.Int, IR.StmtScaf r, r)

tr_arg :: (String, A.Type) -> TM (String, IR.Type)
tr_arg (s, t) = do ir_t <- tmap t
                   ir_name <- getUnusedName s
                   addToEnv s t (IR.Lit ir_name)
                   return (s, ir_t)

tmap :: A.Type -> TM IR.Type
tmap A.Int  = do return IR.Int
tmap A.Bool = do return IR.Bool
tmap t = error $ "Can't map that type (" ++ (show t) ++ ")"

tmatch :: A.Type -> A.Type -> Bool
tmatch p q = p == q

-- This structure describes how to map binary operators. For a given
-- Cemetery operator and the type of each of its two operands, we give
-- the type of the result and the corresponding IR operator.
--
-- In a future, we may use not only IR operators but function calls, or
-- even enable operator polymorphism.
binop_mapping = [
{-
 cmt_op     l_type   r_type   res_type  ir_op
-}
 (A.Plus,   A.Int,   A.Int,   A.Int,    IR.Plus),
 (A.Minus,  A.Int,   A.Int,   A.Int,    IR.Minus),
 (A.Div,    A.Int,   A.Int,   A.Int,    IR.Div),
 (A.Prod,   A.Int,   A.Int,   A.Int,    IR.Prod),
 (A.Mod,    A.Int,   A.Int,   A.Int,    IR.Mod),
 (A.Eq,     A.Int,   A.Int,   A.Int,    IR.Eq),
 (A.And,    A.Bool,  A.Bool,  A.Bool,   IR.And),
 (A.Or,     A.Bool,  A.Bool,  A.Bool,   IR.Or)
 ]

find_matching_binop :: A.BinOp -> A.Type -> A.Type -> TM [(A.Type, IR.BinOp)]
find_matching_binop op l_typ r_typ =
    do let l = filter (\(o, lt, rt, et, ir_op) ->
                op == o && tmatch l_typ lt && tmatch r_typ rt) binop_mapping
       return $ map (\(a,b,c,d,e) -> (d,e)) l

unop_mapping = [
{-
 cmt_op     e_type   res_type  ir_op
-}
 (A.Neg,    A.Int,   A.Int,    IR.Neg),
 (A.Not,    A.Bool,  A.Bool,   IR.Not)
 ]

find_matching_unop :: A.UnOp -> A.Type -> TM [(A.Type, IR.UnOp)]
find_matching_unop op e_typ =
    do let l = filter (\(o, et, rt, ir_op) ->
                op == o && tmatch e_typ et) unop_mapping
       return $ map (\(a,b,c,d) -> (c,d)) l
