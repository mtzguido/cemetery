module CPrint where

import CLang
import Data.List
import Data.Monoid
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Identity

type PM = WriterT [String] Identity

cprint :: Prog -> String
cprint p = let m' = runWriterT (p_prog p)
               (r, lines) = runIdentity m'
            in concat (intersperse "\n" lines)

p_prog :: Prog -> PM ()
p_prog p =
    do mapM p_inc (includes p)
       tell [""]
       mapM p_unit (units p)
       return ()

p_inc :: String -> PM ()
p_inc f = tell $ ["#include <" ++ f ++ ".h>"]

comment s = tell ["/* " ++ s ++ " */"]

p_unit (Decl _) =
    do comment "Decl"

p_unit (FunDecl _) =
    do comment "FunDecl"

p_unit (FunDef _ _) =
    do comment "FunDef"

p_unit (Comment s) =
    do comment s
