module CGen where

import CLang
import Data.List
import Data.Monoid

cgenT :: Prog -> String
cgenT p = concat $ intersperse " " $ fst $ runGM (cgen p)

data GenerateMonad mt a = GM (() -> mt, a)

type Token = String
type GM = GenerateMonad [Token]

instance Monoid mt => Monad (GenerateMonad mt) where
    return v = GM (\_ -> mempty, v)
    GM (f, v) >>= g = let s = f ()
                          GM (h, v2) = g v -- :: m b
                          t = h ()
                       in GM (\_ -> mappend s t, v2)

emit :: String -> GM ()
emit s = GM (\() -> [s], ())

box :: GM () -> GM [Token]
box (GM (m, ())) = do return (m ())

runGM :: GM a -> ([Token], a)
runGM (GM (f, a)) = (f (), a)

cgen :: Prog -> GM ()
cgen p = do bs <- mapM g_unit p
            return ()

g_unit :: Unit -> GM ()
g_unit (Decl d) =
    do g_decl d
       emit "\n"

g_unit (FunDecl ft) =
    do g_fhdr ft
       emit ";"
       emit "\n"

g_unit (FunDef ft b) =
    do g_fhdr ft
       emit "{"
       g_block b
       emit "}"
       emit "\n"

g_block b =
    do let (ds, s) = b
       mapM g_decl ds
       g_stmt s

g_fhdr :: Funtype -> GM ()
g_fhdr ft =
    do t <- box (g_typ (ret ft))
       mapM emit t
       emit (name ft)
       emit "("
       arg_def (args ft)
       emit ")"

arg_def :: [(String, Type)] -> GM ()
arg_def [] = do return ()
arg_def [(n,t)] =
    do g_typ t
       emit n
arg_def (h:t) =
    do arg_def [h]
       emit ","
       arg_def t

g_decl :: Decl -> GM ()
g_decl (VarDecl s t me _) =
    do n <- box (g_typ t)
       mapM emit n
       emit s
       case me of
        Just e -> do emit "="
                     g_expr e
        Nothing -> return ()
       emit ";"

g_stmt :: Stmt -> GM ()
g_stmt (Seq s1 s2) =
    do g_stmt s1
       g_stmt s2

g_stmt (Assign s e) =
    do emit s
       emit "="
       g_expr e
       emit ";"

g_stmt (Return e) =
    do emit "return"
       g_expr e
       emit ";"

g_stmt (If c t e) =
    do emit "if"
       emit "("
       g_expr c
       emit ")"
       emit "{"
       g_block t
       emit "}"
       emit "else"
       emit "{"
       g_block e
       emit "}"

g_stmt Skip =
    do return ()

g_expr (BinOp o l r) =
    do emit "("
       g_expr l
       g_binop o
       g_expr r
       emit ")"

g_expr (UnOp o e) =
    do emit "("
       g_unop o
       g_expr e
       emit ")"

g_expr (ConstInt i) =
    do emit (show i)

g_expr (ConstFloat d) =
    do emit (show d)

g_expr (Call s args) =
    do emit s
       emit "("
       g_args args
       emit ")"

g_expr (Var s) =
    do emit s

g_expr (ConstBool b) =
    do case b of
         True -> emit "true"
         False -> emit "false"

g_expr (ConstStr s) =
    do emit $ "\"" ++ s ++ "\""

g_expr (ConstArr b) =
    do emit "{"
       arr_print b
       emit "}"

arr_print [] = return ()

arr_print [x] =
    do emit $ show x

arr_print (x:xs) =
    do emit $ show x
       emit ","
       arr_print xs

g_args :: [Expr] -> GM ()
g_args [] = do return ()
g_args [h] = do g_expr h
g_args (h:t) =
    do g_expr h
       emit ","
       g_args t

g_typ :: Type -> GM ()
g_typ Int           = do emit "int"
g_typ Double        = do emit "double"
g_typ Void          = do emit "void"
g_typ String        = do emit "char *"
g_typ Bool          = do emit "bool"
g_typ CmtBuf        = do emit "cmt_buf_t"
g_typ _             = do emit "weird_t"

g_binop :: BinOp -> GM ()
g_binop Plus        = do emit "+"
g_binop Minus       = do emit "-"
g_binop Prod        = do emit "*"
g_binop Div         = do emit "/"
g_binop Mod         = do emit "%"
g_binop Eq          = do emit "=="

g_unop :: UnOp -> GM ()
g_unop NegateNum    = do emit "-"
