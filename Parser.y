{
module Parser where

import qualified Data.ByteString as B
import qualified Lexer as L
import qualified AST as A
}

%name cmtParse
%tokentype { L.Token }
%error { \s -> error $ "Parse error near token: " ++ show (head s) }

%token
	ASTERISK	{ L.Tok L.Asterisk _ }
	BRACE		{ L.Tok L.Brace _ }
	BREAK		{ L.Tok L.Break _ }
	CIRC		{ L.Tok L.Circ _ }
	COLON		{ L.Tok L.Colon _ }
	COMMA		{ L.Tok L.Comma _ }
	CONST		{ L.Tok L.Const _ }
	DASH		{ L.Tok L.Dash _ }
	ELSE		{ L.Tok L.Else _ }
	EQ2		{ L.Tok L.Eq2 _ }
	EQ		{ L.Tok L.Eq _ }
	EXTERNAL	{ L.Tok L.External _ }
	FUN		{ L.Tok L.Fun _ }
	ID		{ L.Tok (L.Ident _) _ }
	IF		{ L.Tok L.If _ }
	INT		{ L.Tok (L.IntLit _) _ }
	LANGLE		{ L.Tok L.Langle _ }
	PAREN		{ L.Tok L.Paren _ }
	PERC		{ L.Tok L.Perc _ }
	PLUS		{ L.Tok L.Plus _ }
	RANGLE		{ L.Tok L.Rangle _ }
	RETURN		{ L.Tok L.Return _ }
	SLASH		{ L.Tok L.Slash _ }
	STRING		{ L.Tok (L.StringLit _) _ }
	STRUCT		{ L.Tok L.Struct _ }
	TYPE		{ L.Tok (L.Type _) _ }
	UNBRACE		{ L.Tok L.Unbrace _ }
	UNPAREN		{ L.Tok L.Unparen _ }
	VAR		{ L.Tok L.Var _ }

	PLUSASSIGN	{ L.Tok L.PlusAssign _ }
	MINUSASSIGN	{ L.Tok L.MinusAssign _ }
	PRODASSIGN	{ L.Tok L.ProdAssign _ }
	DIVASSIGN	{ L.Tok L.DivAssign _ }
	XORASSIGN	{ L.Tok L.XorAssign _ }

	EOF		{ L.EOF }

%%

Prog : gdecls funs EOF	{ ($1, $2) }

gdecls : {- empty -}	{ [] }
       | decl gdecls	{ $1 : $2 }

funs : fun		{ $1 : [] }
     | fun funs		{ $1 : $2 }

ig_br : {- empty -}		{ () }
      | BREAK ig_br		{ () }

id : ID { readIdent $1 }
type : TYPE { readType $1 }
intlit : INT { readInt $1 }
strlit : STRING { readStr $1 }

fun : ig_br FUN id PAREN args UNPAREN COLON type
      BRACE stmts UNBRACE	{ A.FunDecl {
				    A.name = $3,
				    A.ret = $8,
				    A.args = $5,
				    A.body = $10 } }

args : {- empty -}		{ [] }
     | arg			{ $1 }
     | arg COMMA args		{ $1 ++ $3 }

arg : idents COLON type		{ zip $1 (repeat $3) }

idents : id			{ $1 : [] }
       | id idents		{ $1 : $2 }

stmts : {- empty -}		{ A.Skip }
      | stmt stmts		{ sseq $1 $2 }

stmt : id EQ expr BREAK		{ A.Assign $1 $3 }
     | RETURN expr BREAK	{ A.Return $2 }
     | BREAK			{ A.Skip }
     | decl			{ A.Decl $1 }
     | BRACE stmts UNBRACE	{ $2 }
     | IF expr stmts		{ A.If $2 $3 A.Skip }
     | IF expr stmts ELSE stmts	{ A.If $2 $3 $5 }
     | id abbrev_op expr	{ A.Assign $1 (A.BinOp $2 (A.Var $1) $3) }

abbrev_op : PLUSASSIGN		{ A.Plus }
          | MINUSASSIGN		{ A.Minus }
          | PRODASSIGN		{ A.Prod }
          | DIVASSIGN		{ A.Div }
          | XORASSIGN		{ A.Xor }

decl : VAR id BREAK		{ A.DeclareAuto $2 }
     | VAR id EQ expr BREAK	{ A.Declare $2 $4 }
     | VAR id COLON type BREAK	{ A.DeclareAutoT $2 $4 }
     | VAR id COLON type
         EQ expr BREAK		{ A.DeclareT $2 $6 $4 }
     | EXTERNAL id COLON
         type BREAK		{ A.External $2 $4 }
     | CONST id EQ expr BREAK	{ A.Const $2 $4 }
     | STRUCT id
         BRACE fields UNBRACE	{ A.Struct }

fields : field			{ $1 : [] }
       | field fields		{ $1 : $2 }

field : id COLON type BREAK	{ 1 }

binlit : LANGLE bytes RANGLE	{ $2 }

bytes : intlit bytes		{ $1 : $2 }
      | {- empty -}		{ [] }

expr : intlit			{ A.ConstInt $1 }
     | id			{ A.Var $1 }
     | binlit			{ A.BinLit $ B.pack $ map fromIntegral $1 }
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
      | PERC		{ A.Mod }
      | CIRC		{ A.Xor }

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
