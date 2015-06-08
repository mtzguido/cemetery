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
    do mapM p_decl decls
       p_stmt stmt

p_decl (VarDecl n t me mods) =
    do t <- p_typ t
       line $ t ++ " " ++ n ++ ";"

p_stmt (Seq l r) =
    do p_stmt l
       p_stmt r

p_stmt Skip =
    do return ()

p_stmt (If c t (_, Skip)) =
    do cc <- p_expr c
       line ("if (" ++ cc ++ ") {")
       indent $ p_block t
       line "}"

p_stmt (If c t e) =
    do cc <- p_expr c
       line ("if (" ++ cc ++ ") {")
       indent $ p_block t
       line "} else {"
       indent $ p_block e
       line "}"

p_stmt (Assign n e) =
    do ee <- p_expr e
       line $ n ++ " = " ++ ee ++ ";"

p_stmt (Return e) =
    do ee <- p_expr e
       line $ "return " ++ ee ++ ";"

p_typ Int = do return "int"
p_typ Bool = do return "bool"

p_expr (BinOp op l r) =
    do oo <- p_binop op
       ll <- p_expr l
       rr <- p_expr r
       return $ paren (ll ++ " " ++ oo ++ " " ++ rr)

p_expr (UnOp op l) =
    do oo <- p_unop op
       ll <- p_expr l
       return $ paren (oo ++ " " ++ ll)

p_expr (ConstInt i) =
    do return $ show i

p_expr (ConstBool b) =
    do return $ if b == True then "true" else "false"

p_expr (ConstFloat f) =
    do return $ show f

p_expr (Call s args) =
    do as <- mapM p_expr args
       return $ paren (s ++ "(" ++ concat (intersperse "," as) ++ ")")

p_expr (Var s) =
    do return s

paren s = "(" ++ s ++ ")"

p_args l =
    do as <- mapM p_arg l
       return $ concat (intersperse ", " as)

p_arg (n, t) =
    do tt <- p_typ t
       return $ tt ++ " " ++ n

p_binop Plus  = do return "+"
p_binop Minus = do return "-"
p_binop Prod  = do return "*"
p_binop Div   = do return "/"
p_binop Eq    = do return "=="
p_binop Mod   = do return "%"
p_binop And   = do return "&&"
p_binop Or    = do return "||"

p_unop NegateNum = do return "-"
p_unop Not       = do return "!"
