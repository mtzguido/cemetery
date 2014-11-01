{
module Lexer (Token(..), alexScanTokens) where

import qualified AST
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum_ = [$alpha '_' $digit]
@ident = $alpha $alnum_*

tokens :-
  $white+		;
  "--".*		;

  fun			{ \p s -> Fun p }
  var			{ \p s -> Var p }
  const			{ \p s -> Const p }
  external		{ \p s -> External p }
  struct		{ \p s -> Struct p }

  "("			{ \p s -> OpenParen p }
  ")"			{ \p s -> CloseParen p }

  "+"			{ \p s -> Plus p }
  "-"			{ \p s -> Dash p }
  "*"			{ \p s -> Asterisk p }
  "/"			{ \p s -> Slash p }
  "^"			{ \p s -> Circ p }
  ","			{ \p s -> Comma p }
  "="			{ \p s -> Eq p }
  "=="			{ \p s -> Eq2 p }
  ":"			{ \p s -> Colon p }

  -- This needs to be extended to multiline strings
  \"[^\"]*\"		{ \p s -> StringLit p (init $ tail $ s) }

  $digit+		{ \p s -> IntLit p (read s) }
  @ident		{ \p s -> ident_or_type s p }
  .			{ \p s -> error $ "unexpected: " ++ s }

{

type PP = AlexPosn

data Token =
  Fun PP | Var PP | Const PP |
  External PP | Struct PP |

  Int PP | Bool PP | Float PP | Bytes PP |
  Ident PP String |

  Plus PP | Dash PP | Asterisk PP | Slash PP | Circ PP |
  Eq PP | Eq2 PP |

  Type PP AST.Type |

  OpenParen PP | CloseParen PP |
  Comma PP | Colon PP |

  IntLit PP Int | StringLit PP String
 deriving (Show)

ident_or_type s p = case lookup s AST.cmtTypeTable of
		      Just t -> Type p t
		      Nothing -> Ident p s

}
