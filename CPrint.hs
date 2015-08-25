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
                              let w' = map (\s -> if s /= "" then '\t':s else s) w
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

p_fun_proto ft =
    do let p = if elem Static (mods ft) then "static " else ""
       rt <- p_typ (ret ft)
       as <- p_args (args ft)
       return $ p ++ rt ++ " " ++ name ft ++ "(" ++ as ++ ")"

p_unit (Decl d@(VarDecl n t me _)) =
    do p_decl d

p_unit (FunDecl ft) =
    do p <- p_fun_proto ft
       line $ p ++ ";"

p_unit (FunDef ft b) =
    do line ""
       p <- p_fun_proto ft
       line p
       line "{"
       indent $ p_block b
       line "}"

summarize []  = []
summarize [d] = [[d]]
summarize (l:r:ds) =
    let VarDecl n  t  me  mods  = l
        VarDecl n' t' me' mods' = r
        s:ss = summarize (r:ds)
     in if t == t' && me == Nothing && me' == Nothing && mods == []
        && mods' == []
            then (l:s):ss
            else [l]:s:ss

p_block ([], stmt) =
    do p_stmt stmt

p_block (decls, stmt) =
    do mapM p_sum_decl (summarize decls)
       line ""
       p_stmt stmt

dname (VarDecl n _ _ _) = n
dtype (VarDecl _ t _ _) = t

p_sum_decl [d] = p_decl d
p_sum_decl [] = error "wat"
p_sum_decl ds =
    do let names = map dname ds
           typ = dtype (head ds)
       tv <- p_typed_var "" typ
       line $ tv ++ (commas names) ++ ";"

p_decl (VarDecl n t me mods) =
    do init <- case me of
                 Nothing -> do return ""
                 Just e -> do (e', _) <- p_expr e
                              return $ " = " ++ e'

       tv <- p_typed_var n t
       line $ tv ++ init ++ ";"

p_stmt (Seq l r) =
    do p_stmt l
       p_stmt r

p_stmt (Comment s) =
    do comment s

p_stmt Skip =
    do return ()

p_stmt s@(If _ _ _) =
    do p_if "" s

p_stmt (Expr e) =
    do (e', _) <- p_expr e
       line $ e' ++ ";"

p_stmt (Return e) =
    do (ee, _) <- p_expr e
       line $ "return " ++ ee ++ ";"

p_stmt (For s c i b) =
    do (ss, _) <- p_expr s
       (cc, _) <- p_expr c
       (ii, _) <- p_expr i
       line $ "for (" ++ ss ++ "; " ++ cc ++ "; " ++ ii ++ ") {"
       indent $ p_block b
       line "}"

p_typ Int = do return "int"
p_typ Bool = do return "bool"
p_typ UChar = do return "unsigned char"
p_typ (Custom s) = do return s

data Assoc = L | R | N
  deriving (Show, Eq)

b_prec Member = (1, L)

b_prec Prod  = (3, L)
b_prec Div   = (3, L)
b_prec Mod   = (3, L)

b_prec Plus  = (4, L)
b_prec Minus = (4, L)

b_prec Lt    = (6, L)
b_prec Le    = (6, L)
b_prec Gt    = (6, L)
b_prec Ge    = (6, L)

b_prec Eq    = (7, L)
b_prec Neq   = (7, L)

b_prec Band  = (8, N)
b_prec Xor   = (8, N)
b_prec Bor   = (8, N)

b_prec And   = (11, L)
b_prec Or    = (12, L)

b_prec Assign = (14, R)

u_prec NegateNum = (2, R)
u_prec Not       = (2, R)
u_prec Bnot      = (2, R)
u_prec Address   = (2, R)

p_expr (BinOp op l r) =
    do oo <- p_binop op
       (ll, lp) <- p_expr l
       (rr, rp) <- p_expr r
       let (p, a) = b_prec op

       let ls = if p < lp || (p == lp && a /= L)
                then paren ll
                else ll

       let rs = if p < rp || (p == rp && a /= R)
                then paren rr
                else rr

       return (ls ++ oo ++ rs, p)

p_expr (UnOp op l) =
    do oo <- p_unop op
       (ll, lp) <- p_expr l
       let (p, a) = u_prec op

       let ls = if p < lp || (p == lp && a /= L)
                then paren ll
                else ll

       return (oo ++ ls, p)

p_expr (ConstInt i) =
    do return (show i, 0)

p_expr (ConstBool b) =
    do return (if b == True then "true" else "false", 0)

p_expr (ConstFloat f) =
    do return (show f, 0)

p_expr (Call s args) =
    do (as, _) <- liftM unzip $ mapM p_expr args
       return (s ++ "(" ++ commas as ++ ")", 0)

p_expr (LV lv) =
    do lvs <- p_lvalue lv
       return (lvs, 0)

p_expr (Arr es) =
    do (p_es, _) <- liftM unzip $ mapM p_expr es
       return (brace $ commas p_es, 0)

p_expr (ConstStr s) =
    do return ("\"" ++ s ++ "\"", 0)

p_expr (StructVal attrs) =
    do ts <- mapM p_attr attrs
       return (brace (commas ts), 0)

p_attr (name, expr) =
    do (e, _) <- p_expr expr
       return $ "." ++ name ++ " = " ++ e

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
p_typed_var n UChar =
    do return $ "unsigned char " ++ n
p_typed_var n (Custom s) =
    do return $ s ++ " " ++ n
p_typed_var n (ArrT t Nothing) =
    do tv <- p_typed_var n t
       return $ tv ++ "[]"
p_typed_var n (ArrT t (Just l)) =
    do tv <- p_typed_var n t
       return $ tv ++ "[" ++ show l ++ "]"

p_if lead (If c t (_, Skip)) =
    do (cc, _) <- p_expr c
       line (lead ++ "if (" ++ cc ++ ") {")
       indent $ p_block t
       line "}"

p_if lead (If c t ([], i@(If _ _ _))) =
    do (cc, _) <- p_expr c
       line (lead ++ "if (" ++ cc ++ ") {")
       indent $ p_block t
       p_if "} else " i

p_if lead (If c t e) =
    do (cc, _) <- p_expr c
       line (lead ++ "if (" ++ cc ++ ") {")
       indent $ p_block t
       line "} else {"
       indent $ p_block e
       line "}"


p_binop Plus   = do return " + "
p_binop Minus  = do return " - "
p_binop Prod   = do return " * "
p_binop Div    = do return " / "
p_binop Eq     = do return " == "
p_binop Neq    = do return " != "
p_binop Mod    = do return " % "
p_binop And    = do return " && "
p_binop Or     = do return " || "
p_binop Lt     = do return " < "
p_binop Gt     = do return " > "
p_binop Le     = do return " <= "
p_binop Ge     = do return " >= "
p_binop Assign = do return " = "
p_binop Band   = do return " & "
p_binop Xor    = do return " ^ "
p_binop Bor    = do return " | "
p_binop Member = do return "->"

p_unop NegateNum = do return "-"
p_unop Not       = do return "!"
p_unop Bnot      = do return "~"
p_unop Address   = do return "&"

p_lvalue (LVar s) =
    do return s

p_lvalue (Access a i) =
    do (aa, _) <- p_expr a
       (ii, _) <- p_expr i
       return $ aa ++ square ii
