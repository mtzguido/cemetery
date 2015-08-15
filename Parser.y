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
	AMP		{ L.Tok L.Amp _ }
	AND		{ L.Tok L.And _ }
	ASTERISK	{ L.Tok L.Asterisk _ }
	BOOL		{ L.Tok (L.BoolLit _) _ }
	BRACE		{ L.Tok L.Brace _ }
	BREAK		{ L.Tok L.Break _ }
	CIRC		{ L.Tok L.Circ _ }
	COLON2		{ L.Tok L.Colon2 _ }
	COLON		{ L.Tok L.Colon _ }
	COMMA		{ L.Tok L.Comma _ }
	CONCAT		{ L.Tok L.Concat _ }
	CONST		{ L.Tok L.Const _ }
	DASH		{ L.Tok L.Dash _ }
	DOT2		{ L.Tok L.Dot2 _ }
	ELSE		{ L.Tok L.Else _ }
	ERROR		{ L.Tok L.Error _ }
	EQ2		{ L.Tok L.Eq2 _ }
	EQ		{ L.Tok L.Eq _ }
	EXTERN		{ L.Tok L.Extern _ }
	FLOAT		{ L.Tok (L.FloatLit _) _ }
	FOR		{ L.Tok L.For _ }
	FUN		{ L.Tok L.Fun _ }
	GE		{ L.Tok L.Ge _ }
	ID		{ L.Tok (L.Ident _) _ }
	IF		{ L.Tok L.If _ }
	IN		{ L.Tok L.In _ }
	INT		{ L.Tok (L.IntLit _) _ }
	LANGLE		{ L.Tok L.Langle _ }
	LE		{ L.Tok L.Le _ }
	LROT		{ L.Tok L.LRot _ }
	LSHIFT		{ L.Tok L.LShift _ }
	NE		{ L.Tok L.Ne _ }
	NOT		{ L.Tok L.Not _ }
	OR		{ L.Tok L.Or _ }
	PAREN		{ L.Tok L.Paren _ }
	PERC		{ L.Tok L.Perc _ }
	PIPE		{ L.Tok L.Pipe _ }
	PLUS		{ L.Tok L.Plus _ }
	RANGLE		{ L.Tok L.Rangle _ }
	RETURN		{ L.Tok L.Return _ }
	RROT		{ L.Tok L.RRot _ }
	RSHIFT		{ L.Tok L.RShift _ }
	SLASH		{ L.Tok L.Slash _ }
	SQUARE		{ L.Tok L.Square _ }
	SQUARE2		{ L.Tok L.Square2 _ }
	STATIC		{ L.Tok L.Static _ }
	STRING		{ L.Tok (L.StringLit _) _ }
	TILDE		{ L.Tok L.Tilde _ }
	TYPE		{ L.Tok (L.Type _) _ }
	UNBRACE		{ L.Tok L.Unbrace _ }
	UNPAREN		{ L.Tok L.Unparen _ }
	UNSQUARE	{ L.Tok L.Unsquare _ }
	UNSQUARE2	{ L.Tok L.Unsquare2 _ }
	VAR		{ L.Tok L.Var _ }

	PLUSASSIGN	{ L.Tok L.PlusAssign _ }
	MINUSASSIGN	{ L.Tok L.MinusAssign _ }
	PRODASSIGN	{ L.Tok L.ProdAssign _ }
	DIVASSIGN	{ L.Tok L.DivAssign _ }
	XORASSIGN	{ L.Tok L.XorAssign _ }

	EOF		{ L.EOF }

%left AND OR
%left EQ2 NE LANGLE RANGLE LE GE
%left CONCAT
%left PIPE AMP
%left LSHIFT RSHIFT LROT RROT
%left PLUS DASH
%left ASTERISK SLASH
%left PERC CIRC
%left ELSE
%left NEG TILDE
%left SQUARE
%left SQUARE2
%%

Prog : EOF		{ [] }
     | fun Prog		{ $1 : $2 }
     | decl Prog	{ $1 ++ $2 }
     | BREAK Prog	{ $2 }

id : ID { readIdent $1 }
intlit : INT { readInt $1 }
floatlit : FLOAT { readFloat $1 }
strlit : STRING { readStr $1 }
boollit : BOOL { readBool $1 }
type : TYPE { readType $1 }
     | TYPE SQUARE intlit UNSQUARE { A.ArrT (readType $1) (Just $3) }

fun : funmods FUN id PAREN args UNPAREN COLON type
      BRACE stmts UNBRACE	{ A.FunDecl {
				    A.name = $3,
				    A.ret = $8,
				    A.args = $5,
				    A.mods = $1,
				    A.body = $10 } }

funmods : {- empty -}		{ [] }
        | STATIC funmods	{ A.Static : $2 }

args : {- empty -}		{ [] }
     | arg			{ $1 }
     | arg COMMA args		{ $1 ++ $3 }

arg : idents COLON type		{ zip $1 (repeat $3) }

idents : id			{ $1 : [] }
       | id idents		{ $1 : $2 }

stmts : {- empty -}		{ A.Skip }
      | stmt stmts		{ A.sseq $1 $2 }

stmt_group : BRACE stmts UNBRACE	{ $2 }

vardecl : mods idents var_typ var_init BREAK
				{ map (\n -> A.VarDecl n $1 $3 $4) $2 }

stmt : lv EQ expr BREAK		{ A.Assign $1 $3 }
     | RETURN expr BREAK	{ A.Return $2 }
     | BREAK			{ A.Skip }
     | vardecl			{ foldl1 A.Seq (map A.Decl $1) }
     | stmt_group		{ $1 }
     | if			{ $1 }
     | lv abbrev_op expr BREAK	{ A.Assign $1 (A.BinOp $2 (A.LV $1) $3) }
     | FOR id IN expr DOT2 expr
	stmt_group		{ A.For $2 $4 $6 $7 }
     | ERROR strlit		{ A.Err $2 }

if : IF expr stmt_group	{ A.If $2 $3 A.Skip }
   | IF expr stmt_group ELSE stmt_group
				{ A.If $2 $3 $5 }
   | IF expr stmt_group ELSE if	{ A.If $2 $3 $5 }

abbrev_op : PLUSASSIGN		{ A.Plus }
          | MINUSASSIGN		{ A.Minus }
          | PRODASSIGN		{ A.Prod }
          | DIVASSIGN		{ A.Div }
          | XORASSIGN		{ A.Xor }

mods : VAR			{ [] }
     | CONST mods		{ A.Const : $2 }
     | CONST			{ [A.Const] }
     | EXTERN mods		{ A.Extern : $2 }

var_init : {- empty -}		{ Nothing }
         | EQ expr		{ Just $2 }

var_typ : {- empty -}		{ Nothing }
        | COLON type		{ Just $2 }

decl : vardecl			{ $1 }

binlit : LANGLE bytes COLON2 intlit RANGLE
				{ A.BinLit $2 $4 }
       | LANGLE RANGLE		{ A.BinLit [] 0 }

bytes : intlit bytes		{ (explode $1) ++ $2 }
      | {- empty -}		{ [] }

expr : intlit			{ A.ConstInt $1 }
     | floatlit			{ A.ConstFloat $1 }
     | binlit			{ $1 }
     | lv			{ A.LV $1 }
     | expr PLUS	expr	{ A.BinOp A.Plus	$1 $3 }
     | expr DASH	expr	{ A.BinOp A.Minus	$1 $3 }
     | expr SLASH	expr	{ A.BinOp A.Div		$1 $3 }
     | expr ASTERISK	expr	{ A.BinOp A.Prod	$1 $3 }
     | expr EQ2		expr	{ A.BinOp A.Eq		$1 $3 }
     | expr NE		expr	{ A.UnOp A.Not (A.BinOp A.Eq $1 $3) }
     | expr LE		expr	{ A.BinOp A.Le		$1 $3 }
     | expr GE		expr	{ A.BinOp A.Ge		$1 $3 }
     | expr LANGLE	expr	{ A.BinOp A.Lt		$1 $3 }
     | expr RANGLE	expr	{ A.BinOp A.Gt		$1 $3 }
     | expr PERC	expr	{ A.BinOp A.Mod		$1 $3 }
     | expr CIRC	expr	{ A.BinOp A.Xor		$1 $3 }
     | expr AND		expr	{ A.BinOp A.And		$1 $3 }
     | expr OR		expr	{ A.BinOp A.Or		$1 $3 }
     | expr AMP		expr	{ A.BinOp A.Band	$1 $3 }
     | expr PIPE	expr	{ A.BinOp A.Bor		$1 $3 }
     | expr CONCAT	expr	{ A.BinOp A.BConcat	$1 $3 }
     | expr LSHIFT	expr	{ A.BinOp A.LShift	$1 $3 }
     | expr RSHIFT	expr	{ A.BinOp A.RShift	$1 $3 }
     | expr LROT	expr	{ A.BinOp A.LRot	$1 $3 }
     | expr RROT	expr	{ A.BinOp A.RRot	$1 $3 }
     | id PAREN exprs UNPAREN	{ A.Call $1 $3 }
     | PAREN expr UNPAREN	{ $2 }
     | DASH expr %prec NEG	{ A.UnOp A.Neg $2 }
     | NOT expr %prec NEG	{ A.UnOp A.Not $2 }
     | TILDE expr %prec NEG	{ A.UnOp A.Bnot $2 }
     | strlit			{ A.ConstStr $1 }
     | boollit			{ A.ConstBool $1 }
     | SQUARE exprs UNSQUARE	{ A.Arr $2 }
     | expr SQUARE2 expr COMMA expr UNSQUARE2
				{ A.Slice $1 $3 $5 }

lv : id				{ A.Var $1 }
   | lv SQUARE expr UNSQUARE	{ A.Access $1 $3 }

exprs : {- empty -}			{ [] }
      | BRACE exprs UNBRACE exprs	{ $2 ++ $4 }
      | expr				{ [$1] }
      | expr COMMA exprs		{ $1 : $3 }
      | expr BREAK exprs		{ $1 : $3 }
      | expr COMMA BREAK exprs		{ $1 : $4 }

{

readIdent	(L.Tok (L.Ident s) _) = s
readType	(L.Tok (L.Type t) _) = t
readInt		(L.Tok (L.IntLit i) _) = i
readFloat	(L.Tok (L.FloatLit d) _) = d
readStr		(L.Tok (L.StringLit s) _) = s
readBool	(L.Tok (L.BoolLit b) _) = b

explode i | i >= 0 && i < 256 = [i]
explode i = explode (i `div` 256) ++ explode (i `mod` 256)

}
