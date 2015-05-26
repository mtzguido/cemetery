module CGen where

import Data.List
import Data.Monoid
import IR
import Common

cgen :: IR -> String
cgen p = joinLines $ fst $ runGM (g_ir p)

joinLines ls = concat $ map (++ "\n") ls

data GenerateMonad mt a = GM (() -> mt, a)

type GM = GenerateMonad [String]

instance Monoid mt => Monad (GenerateMonad mt) where
    return v = GM (\_ -> mempty, v)
    GM (f, v) >>= g = let s = f ()
                          GM (h, v2) = g v -- :: m b
                          t = h ()
                       in GM (\_ -> mappend s t, v2)

emit :: String -> GM ()
emit s = GM (\() -> [s], ())

emit' :: [String] -> GM ()
emit' ls = emit (concat ls)

box :: GM () -> GM [String]
box (GM (m, ())) = do return (m ())

runGM :: GM a -> ([String], a)
runGM (GM (f, a)) = (f (), a)

g_ir :: IR -> GM ()
g_ir p = do bs <- mapM g_unit p
            return ()

show_args_decl [] = ""
show_args_decl [(n,t)] = showC t ++ " " ++ n
show_args_decl ((n,t):as) = showC t ++ " " ++ n ++ ", " ++ show_args_decl as

g_unit :: Unit -> GM ()
g_unit (FunDef (Funtype { name = name,
                          args = args,
                          ret  = ret}) body) =
    do emit' [showC ret, " ", name, "(", show_args_decl args, ")"]
       emit "{"
       b <- box $ g_stmt body
       emit $ indent (joinLines b)
       emit "}"

g_unit (UnitScaf) =
    do emit "/* UnitScaf */"

show_args [] = ""
show_args [r] = showC r
show_args (r:rs) = showC r ++ ", " ++ show_args rs

g_stmt :: Stmt -> GM ()
g_stmt (Seq l r) =
    do g_stmt l
       g_stmt r
g_stmt (AssignInt r i) =
    do emit' [showC r, " = ", show i, ";"]
g_stmt (Assign r t) =
    do emit' [showC r, " = ", showC t, ";"]
g_stmt (AssignUnOp op r t) =
    do emit' [showC r, " = ", showC op, showC t, ";"]
g_stmt (AssignBinOp op r s t) =
    do emit' [showC r, " = ", showC s, " ", showC op, " ", showC t, ";"]
g_stmt (Return r) =
    do emit' ["return ", showC r, ";"]
g_stmt (Skip) =
    do return ()
g_stmt (Call n args r) =
    do emit' [showC r, " = ", n, "(", show_args args, ");"]
g_stmt (If r t e) =
    do tt <- box (g_stmt t)
       ee <- box (g_stmt e)
       emit' ["if (", showC r, ") {"]
       emit $ indent (joinLines tt)
       emit $ "} else {"
       emit $ indent (joinLines ee)
       emit $ "}"

g_stmt (StmtScaf _) =
    do emit "/* StmtScaf */"

class ShowC a where
    showC :: a -> String

instance ShowC Type where
    showC Int   = "int"
    showC Bool  = "bool"

instance ShowC UnOp where
    showC Neg   = "-"
    showC Not   = "!"

instance ShowC BinOp where
    showC Plus  = "+"
    showC Minus = "-"
    showC Div   = "/"
    showC Prod  = "*"
    showC Eq    = "=="
    showC Mod   = "%"
    showC And   = "&&"
    showC Or    = "||"

instance ShowC Reg where
    showC (Temp i) = "r" ++ show i
