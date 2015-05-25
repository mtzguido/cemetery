module CGen where

import Data.List
import Data.Monoid
import IR

cgen :: IR -> String
cgen p = concat $ map (++ "\n") $ fst $ runGM (g_ir p)

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

box :: GM () -> GM [String]
box (GM (m, ())) = do return (m ())

runGM :: GM a -> ([String], a)
runGM (GM (f, a)) = (f (), a)

g_ir :: IR -> GM ()
g_ir p = do bs <- mapM g_unit p
            return ()

flatten :: Stmt -> [Stmt]
flatten (Seq l r) = flatten l ++ flatten r
flatten x = [x]

g_unit :: Unit -> GM ()
g_unit (FunDef (Funtype { name = name,
                          args = args,
                          ret  = ret}) body) =
    do let proto = "f()"
       emit proto
       emit "{"
       mapM g_stmt (flatten body)
       emit "}"

g_stmt :: Stmt -> GM ()
g_stmt _ = emit "..."
