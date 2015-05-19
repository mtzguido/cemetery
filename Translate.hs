module Translate where

import qualified CLang as C
import qualified AST as A

import Common
import Control.Exception
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List as L
import Debug.Trace

-- Monad definition

data LevelState = LevelState {
                      env :: M.Map A.VarName (A.Type, A.VarName, C.Type),
                      opening :: [C.Decl],
                      ret_type :: A.Type
                  }
                  deriving (Show)

blank_level :: LevelState
blank_level = LevelState { env = M.empty, opening = [], ret_type = A.Invalid }

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

-- Environment handling functions

getData :: TM [LevelState]
getData = do s <- get
             return $ level_data s

setData :: [LevelState] -> TM ()
setData es = do s <- get
                put $ s { level_data = es }

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
               setData (blank_level : e)

popLevel :: TM LevelState
popLevel = do e <- getData
              setData (tail e)
              return (head e)

minus :: Eq a => [a] -> [a] -> [a]
minus p q = L.filter (\x -> not (elem x q)) p

cdecl_name :: C.Decl -> String
cdecl_name (C.VarDecl s _ _ _) = s

sndOf3 (a,b,c) = b

getUnusedName :: String -> TM String
getUnusedName s = do lvls <- getData
                     let level_env l = L.map (sndOf3.snd) (M.toList (env l))
                     let level_decls l = L.map cdecl_name (opening l)
                     let level_vars l = level_env l ++ level_decls l
                     let used_names = concat $ L.map level_vars lvls
                     let possible_names = s : (L.map (\i -> s ++ "_" ++ show i) [1..])
                     let valid_names = minus possible_names used_names
                     return (head valid_names)

-- Adds a name to the environment, with a hint of how the C name should look like
addToEnv :: A.VarName -> (A.Type, C.Type) -> TM A.VarName
addToEnv n (at, ct) = do nn <- getUnusedName n
                         addToEnv' n (at, nn, ct)
                         return nn

addToEnv' :: A.VarName -> (A.Type, A.VarName, C.Type) -> TM ()
addToEnv' n t = do e:es <- getData
                   setData $ (e { env = M.insert n t (env e) }) : es

addDecl :: C.Decl -> TM ()
addDecl d = do l:ls <- getData
               setData $ l { opening = d : opening l } : ls

addGlobalDecl :: C.Decl -> TM ()
addGlobalDecl d = do ls <- getData
                     let l = last ls
                     setData $ (init ls ++ [l { opening = d : opening l }])

-- C Types don't matter here
builtins :: [(String, String, A.Type, C.Type)]
builtins = [
    ("trunc",   "__cmt_trunc",  A.Fun [A.Bytes] A.Bytes, C.Void),
    ("repeat",  "__cmt_repeat", A.Fun [A.Bytes] A.Bytes, C.Void),
    ("length",  "__cmt_length", A.Fun [A.Bytes] A.Int,   C.Void)
 ]

add_builtins :: TM ()
add_builtins = do mapM (\(an, cn, at, ct) -> addToEnv' an (at, cn, ct))
                       builtins
                  return ()

ff :: Maybe a -> Maybe a -> Maybe a
ff (Just x) _ = Just x
ff Nothing m = m

env_lookup :: String -> TM (A.Type, A.VarName, C.Type)
env_lookup s = do d <- getData
                  let dd = L.map env d
                  let f = Prelude.foldl (\a v -> ff a (M.lookup s v)) Nothing dd
                  case f of
                    Nothing -> error $ "undefined variable: " ++ s
                    Just i -> return i

-- Main translation procedure

translate :: A.Prog -> TM C.Prog
translate decls = do pushLevel -- global environment
                     add_builtins -- add builtins to the environment
                     dd <- mapM tr1 decls
                     final <- popLevel
                     let gd = reverse (opening final)

                     return $ L.map C.Decl gd ++ concat dd

-- Type inference and mapping

infer :: A.Expr -> TM A.Type
infer (A.ConstStr _)    = do return A.String
infer (A.ConstInt _)    = do return A.Int
infer (A.ConstFloat _)  = do return A.Double
infer (A.ConstBool _)   = do return A.Bool
infer (A.BinLit _)      = do return A.Bytes
infer (A.BinOp A.Xor l r) =
    do lt <- infer l
       rt <- infer r
       if not (tmatch lt rt)
       then error "type mismatch (10)"
       else return ()
       return lt

infer (A.Var v)         =
    do (tv, _, __) <- env_lookup v
       return tv
infer (A.Call fun args) =
    do (tf, _, _) <- env_lookup fun
       -- We assume the arguments are OK
       let A.Fun _ rt = tf
       return rt

infer (A.BinOp A.Plus l r) =
    do lt <- infer l
       rt <- infer r
       if is_byte_type lt && is_byte_type rt
       then return lt
       else if is_num_type lt && is_num_type rt
       then return lt
       else error "can't infer"

infer e = do trace ("e = " ++ show e) (error "")

tmap :: A.Type -> TM C.Type
tmap A.Int      = do return C.Int
tmap A.Bool     = do return C.Bool
tmap A.String   = do return C.String
tmap A.Void     = do return C.Void
tmap A.Double   = do return C.Double
tmap A.Bytes    = do return C.CmtBuf

-- Unit translations

tr1 :: A.Decl -> TM [C.Unit]
tr1 (A.VarDecl mods n Nothing Nothing) =
    -- When there's no type nor expression, assume it's an int.
    -- Later, we'll try to infer the type.
    tr1 (A.VarDecl mods n (Just A.Int) Nothing)

tr1 (A.VarDecl n mods (Just typ) Nothing) =
    do tt <- tmap typ
       nn <- addToEnv n (typ, tt)
       return [C.Decl $ C.VarDecl nn tt Nothing []]

tr1 (A.VarDecl n mods Nothing (Just expr)) =
    do ta <- infer expr
       typ <- tmap ta
       (ee, te) <- trexp expr
       if tmatch ta te
           then do nn <- addToEnv n (ta, typ)
                   return [C.Decl $ C.VarDecl nn typ (Just ee) []]
           else error "type mismatch (8)"

tr1 (A.VarDecl n mods (Just ta) (Just expr)) =
    do ta' <- infer expr
       if ta' /= ta
         then throwError CmtErr
         else return ()

       typ <- tmap ta
       (ee, te) <- trexp expr
       if tmatch ta te
           then do nn <- addToEnv n (ta, typ)
                   return [C.Decl $ C.VarDecl nn typ (Just ee) []]
           else error "type mismatch (9)"

tr1 (A.Struct) =
    do return []

tr1 (A.FunDecl { A.name = n, A.ret = r, A.args = a, A.body = b}) =
    do ret_type <- tmap r
       let names_a = L.map fst a
       let argtypes_a = L.map snd a

       argtypes_c <- mapM tmap argtypes_a

       names_c <- mapM getUnusedName names_a

       let args_c = zip names_c argtypes_c
       let funt = C.Funtype { C.name = n, C.args = args_c, C.ret = ret_type }
       fn <- addToEnv n (A.Fun argtypes_a r, C.Fun funt)

       pushLevel
       let add1 ((a, ta), tc, aa) = do addToEnv' a (ta, aa, tc)
       mapM add1 (zip3 a argtypes_c names_c)

       body <- trbody b r
       popLevel
       return [C.FunDef funt body]

-- Statement and expression translations
-- Abandon all hope ye who enter below this line

trbody :: A.Stmt -> A.Type -> TM C.Block
trbody s t =
    do pushLevel
       setRetType t
       st <- trstm s
       lvl <- popLevel
       let ds = opening lvl
       return (reverse ds, st)

fromCDecl :: C.Unit -> C.Decl
fromCDecl (C.Decl d) = d
fromCDecl _ = assert False (error "")

trstm :: A.Stmt -> TM C.Stmt
trstm A.Skip = do return C.Skip

trstm (A.Assign n e) =
    do (ee, te) <- trexp e
       (ta, nn, __) <- env_lookup n

       if tmatch ta te
         then return (C.Assign nn ee)
         else error "type mismatch (1)"

trstm (A.Seq l r) =
    do ll <- trstm l
       rr <- trstm r
       return $ C.sseq ll rr

trstm (A.Return e) =
    do (ee, te) <- trexp e
       rt <- getRetType
       if tmatch te rt
         then return (C.Return ee)
         else error "type mismatch (2)"

trstm (A.Decl d) =
    do cds <- tr1 d
       mapM addDecl (L.map fromCDecl cds)
       return C.Skip

trstm (A.If c t e) =
    do (cc, tc) <- trexp c
       tr <- getRetType
       trace ("lala:" ++ show c ++ "::" ++ show cc ++ "::" ++ show tc) (return ())
       if tmatch tc A.Bool || tmatch tc A.Int
         then do tt <- trbody t tr
                 ee <- trbody e tr
                 return (C.If cc tt ee)
         else error "type mismatch (3)"

trexp :: A.Expr -> TM (C.Expr, A.Type)
trexp (A.ConstInt n) =
    do return (C.ConstInt n, A.Int)

trexp (A.ConstFloat f) =
    do return (C.ConstFloat f, A.Double)

trexp (A.ConstBool b) =
    do return (C.ConstBool b, A.Bool)

trexp (A.ConstStr s) =
    do return (C.ConstStr s, A.String)

trexp (A.BinOp a_op l r) =
    do (ll, tl) <- trexp l
       (rr, tr) <- trexp r
       trbinop a_op ll tl rr tr

trexp (A.UnOp a_op e) =
    do (ee, te) <- trexp e
       trunop a_op ee te

trexp (A.Call f args) =
    do (tf, ff, _) <- env_lookup f
       args_ir <- mapM trexp args
       let (args, args_t) = unzip args_ir
       trace ("looking for " ++ f) (return ())
       let A.Fun _ rt = tf
       -- check match for f(a1, a2, ...)
       return (C.Call ff args, rt)

trexp (A.Var v) =
    do (tv, vv, _) <- env_lookup v
       return (C.Var v, tv)

-- This declares the struct and the pointer to it,
-- thus, there are two names.
trexp (A.BinLit b) =
    do ce <- bin_init b

       str_name <- getUnusedName "__cmt_litbuf"
       let str_type = (C.CmtBufStruct)
       let str_init = Just ce
       let str_mods = [C.Static]

       p_name <- getUnusedName "__cmt_litbufp"
       let p_type = C.CmtBuf
       let p_init = Just (C.PtrTo str_name)
       let p_mods = [C.Const]

       addGlobalDecl (C.VarDecl str_name str_type str_init str_mods);
       addGlobalDecl (C.VarDecl p_name p_type p_init p_mods);
       return (C.Var p_name, A.Bytes)

bin_init :: B.ByteString -> TM C.Expr
bin_init b = do let bs = B.unpack b
                let str = C.StructVal [("length", C.ConstInt (length bs)),
                                       ("data", C.ConstArr bs)]
                return str

-- Operator translation

trbinop A.Plus  = tr_plus
trbinop A.Minus = arith_op C.Minus
trbinop A.Prod  = arith_op C.Prod
trbinop A.Div   = arith_op C.Div
trbinop A.Mod   = arith_op C.Mod
trbinop A.Xor   = tr_xor
trbinop A.Eq    = tr_comparison

arith_op op l lt r rt =
    do if (tmatch lt A.Int && tmatch rt A.Int)
          || (tmatch lt A.Double && tmatch rt A.Double)
           then return (C.BinOp op l r, lt)
           else error "type mismatch (4)"

tr_plus l lt r rt =
    do if is_byte_type lt && is_byte_type rt
       then return (C.Call "__cmt_append" [l, r], A.Bytes)
       else arith_op C.Plus l lt r rt

tr_xor l lt r rt =       -- built-in operator
    do if is_byte_type lt && is_byte_type rt
         then return (C.Call "__cmt_xor" [l, r], lt)
         else error "type mismatch (5)"

tr_comparison l lt r rt =
    do if tmatch lt rt -- obviously wrong once we have richer types
           then return (C.BinOp C.Eq l r, A.Bool)
           else error "type mismatch (6)"

trunop A.NegateNum  = tr_negatenum

tr_negatenum ee te =
    do if tmatch te A.Int
           then return (C.UnOp C.NegateNum ee, te)
           else error "type mismatch (7)"

tmatch :: A.Type -> A.Type -> Bool
tmatch p q = p == q

is_byte_type :: A.Type -> Bool
is_byte_type t = tmatch t A.Bytes

is_num_type :: A.Type -> Bool
is_num_type t = tmatch t A.Int || tmatch t A.Double
