{
module Parser where

import qualified Lexer as L
import qualified AST as A
}

%name cmtParse
%tokentype { L.Token }
%error { \s -> error $ "Parse error near token: " ++ show (head s) }

%token
	FUN		{ L.Tok L.Fun _ }
	ID		{ L.Tok (L.Ident _) _ }
	PAREN		{ L.Tok L.Paren _ }
	UNPAREN		{ L.Tok L.Unparen _ }
	COLON		{ L.Tok L.Colon _ }
	TYPE		{ L.Tok (L.Type _) _ }
	BREAK		{ L.Tok L.Break _ }
	BRACE		{ L.Tok L.Brace _ }
	UNBRACE		{ L.Tok L.Unbrace _ }
	EQ		{ L.Tok L.Eq _ }
	EQ2		{ L.Tok L.Eq2 _ }
	INT		{ L.Tok (L.IntLit _) _ }
	STRING		{ L.Tok (L.StringLit _) _ }
	COMMA		{ L.Tok L.Comma _ }
	PLUS		{ L.Tok L.Plus _ }
	DASH		{ L.Tok L.Dash _ }
	SLASH		{ L.Tok L.Slash _ }
	ASTERISK	{ L.Tok L.Asterisk _ }
	RETURN		{ L.Tok L.Return _ }
	IF		{ L.Tok L.If _ }
	ELSE		{ L.Tok L.Else _ }
	VAR		{ L.Tok L.Var _ }
	EOF		{ L.EOF }

%%

Prog : funs EOF 	{ $1 :: A.Prog }

funs : fun		{ $1 : [] }
     | fun funs		{ $1 : $2 }

id : ID { readIdent $1 }
type : TYPE { readType $1 }
intlit : INT { readInt $1 }
strlit : STRING { readStr $1 }

fun : FUN id PAREN args UNPAREN COLON type
      BRACE stmts UNBRACE	{ A.FunDecl {
				    A.name = $2,
				    A.ret = $7,
				    A.args = $4,
				    A.body = $9 } }

args : {- empty -}		{ [] }
     | arg			{ $1 }
     | arg COMMA args		{ $1 ++ $3 }

arg : idents COLON type		{ zip $1 (repeat $3) }

idents : id			{ $1 : [] }
       | id idents		{ $1 : $2 }

stmts : {- empty -}		{ A.Skip }
      | stmt stmts		{ sseq $1 $2 }

stmt : id EQ expr BREAK		{ A.Assign $1 $3 }
     | VAR id BREAK		{ A.DeclareAuto $2 }
     | VAR id EQ expr BREAK	{ A.Declare $2 $4 }
     | RETURN expr BREAK	{ A.Return $2 }
     | BREAK			{ A.Skip }
     | BRACE stmts UNBRACE	{ $2 }
     | IF expr stmts		{ A.If $2 $3 A.Skip }
     | IF expr stmts ELSE stmts	{ A.If $2 $3 $5 }

expr : intlit			{ A.ConstInt $1 }
     | id			{ A.Var $1 }
     | expr binop expr		{ A.BinOp $2 $1 $3 }
     | id PAREN argv UNPAREN	{ A.Call $1 $3 }
     | PAREN expr UNPAREN	{ $2 }
     | unop expr		{ A.UnOp $1 $2 }
     | strlit			{ A.ConstStr $1 }

argv : {- empty -}		{ [] }
     | expr			{ [$1] }
     | expr COMMA argv		{ $1 : $3 }

binop : PLUS		{ A.Plus }
      | DASH		{ A.Minus }
      | SLASH		{ A.Div }
      | ASTERISK	{ A.Prod }
      | EQ2		{ A.Eq }

unop : DASH		{ A.NegateNum }

{

sseq A.Skip s = s
sseq s A.Skip = s
sseq s t = A.Seq s t

readIdent	(L.Tok (L.Ident s) _) = s
readType	(L.Tok (L.Type t) _) = t
readInt		(L.Tok (L.IntLit i) _) = i
readStr		(L.Tok (L.StringLit s) _) = s

}
