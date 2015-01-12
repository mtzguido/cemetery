{
module Parser where

import qualified Lexer as L
import qualified AST as A
}

%name cmtParse
%tokentype { L.Token }
%error { \_ -> error "wat" }

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
	INT		{ L.Tok (L.IntLit n) _ }
	COMMA		{ L.Tok L.Comma _ }
	EOF		{ L.EOF }

%%

Prog : funs EOF 	{ $1 :: A.Prog }

funs : fun		{ $1 : [] }
     | fun funs		{ $1 : $2 }

id : ID { readIdent $1 }
type : TYPE { readType $1 }
intlit : INT { readInt $1 }

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

stmts : 			{ A.Skip }
      | stmt stmts		{ A.Seq $1 $2 }

stmt : id EQ intlit BREAK	{ A.Assign $1 (A.ConstInt $3) }

{

readIdent (L.Tok (L.Ident s) _) = s
readType (L.Tok (L.Type t) _) = t
readInt (L.Tok (L.IntLit i) _) = i

}
