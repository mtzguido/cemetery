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

line s = tell [s]

indent :: PM a -> PM a
indent (WriterT { runWriterT = r }) =
    WriterT { runWriterT = do (a, w) <- r
                              let w' = map ('\t':) w
                              return (a, w') }

p_prog :: Prog -> PM ()
p_prog p =
    do mapM p_inc (includes p)
       line ""
       mapM p_unit (units p)
       return ()

p_inc :: String -> PM ()
p_inc f = line ("#include <" ++ f ++ ".h>")

comment s = line ("/* " ++ s ++ " */")

p_unit (Decl _) =
    do comment "Decl"

p_unit (FunDecl _) =
    do comment "FunDecl"

p_unit (Comment s) =
    do comment s

p_unit (FunDef ft b) =
    do rt <- p_typ (ret ft)
       as <- p_args (args ft)
       line (rt ++ " " ++ name ft ++ "(" ++ as ++ ")")
       line "{"
       indent $ p_block b
       line "}"

p_block (decls, stmt) =
    do p_stmt stmt

p_stmt (Seq l r) =
    do p_stmt l
       p_stmt r

p_stmt (If c t e) =
    do cc <- p_expr c
       line ("if (" ++ cc ++ ") {")
       indent $ p_block t
       line "} else {"
       indent $ p_block e
       line "}"

p_stmt (_) =
    do comment "stmt"

p_typ Int = do return "int"
p_typ Bool = do return "bool"

p_expr _ = do return "magic expr"

p_args l =
    do as <- mapM p_arg l
       return $ concat (intersperse ", " as)

p_arg (n, t) =
    do tt <- p_typ t
       return $ tt ++ " " ++ n
