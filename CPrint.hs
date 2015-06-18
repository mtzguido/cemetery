module CPrint where

import CLang
import Data.List
import Data.Monoid
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Identity

type PM = WriterT [String] Identity

cprint :: String -> Prog -> String
cprint prologue p = let m' = runWriterT (p_prog prologue p)
                        (r, lines) = runIdentity m'
                     in concat (intersperse "\n" lines)

line s = tell [s]

indent :: PM a -> PM a
indent (WriterT { runWriterT = r }) =
    WriterT { runWriterT = do (a, w) <- r
                              let w' = map ('\t':) w
                              return (a, w') }

p_prog :: String -> Prog -> PM ()
p_prog prologue p =
    do mapM p_inc (includes p)
       line ""
       tell (lines prologue)
       line ""
       mapM p_unit (units p)
       return ()

p_inc :: String -> PM ()
p_inc f = line ("#include <" ++ f ++ ".h>")

comment s = line ("/* " ++ s ++ " */")

p_unit (Decl d@(VarDecl n t me _)) =
    do p_decl d

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
    do init <- case me of
                 Nothing -> do return ""
                 Just e -> do e' <- p_expr e
                              return $ " = " ++ e'

       tv <- p_typed_var n t
       line $ tv ++ init ++ ";"

p_stmt (Seq l r) =
    do p_stmt l
       p_stmt r

p_stmt Skip =
    do return ()

p_stmt s@(If _ _ _) =
    do p_if "" s

p_stmt (Assign n e) =
    do ee <- p_expr e
       line $ n ++ " = " ++ ee ++ ";"

p_stmt (Return e) =
    do ee <- p_expr e
       line $ "return " ++ ee ++ ";"

p_typ Int = do return "int"
p_typ Bool = do return "bool"
p_typ (Custom s) = do return s

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
       return $ paren (s ++ "(" ++ commas as ++ ")")

p_expr (Var s) =
    do return s

p_expr (Arr es) =
    do p_es <- mapM p_expr es
       return $ brace $ commas p_es

p_expr (Access a i) =
    do aa <- p_expr a
       ii <- p_expr i
       return $ aa ++ square ii

paren s = "(" ++ s ++ ")"
brace s = "{" ++ s ++ "}"
square s = "[" ++ s ++ "]"

commas ss = concat $ intersperse ", " ss

p_args l =
    do as <- mapM (uncurry p_typed_var) l
       return $ commas as

p_typed_var n Int =
    do return $ "int " ++ n
p_typed_var n Bool =
    do return $ "bool " ++ n
p_typed_var n (Custom s) =
    do return $ s ++ " " ++ n
p_typed_var n (ArrT t) =
    do tv <- p_typed_var n t
       return $ tv ++ "[]"

p_if lead (If c t (_, Skip)) =
    do cc <- p_expr c
       line (lead ++ "if (" ++ cc ++ ") {")
       indent $ p_block t
       line "}"

p_if lead (If c t ([], i@(If _ _ _))) =
    do cc <- p_expr c
       line (lead ++ "if (" ++ cc ++ ") {")
       indent $ p_block t
       p_if "} else " i

p_if lead (If c t e) =
    do cc <- p_expr c
       line (lead ++ "if (" ++ cc ++ ") {")
       indent $ p_block t
       line "} else {"
       indent $ p_block e
       line "}"


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
