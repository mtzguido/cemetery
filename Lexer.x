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

  "("			{ pos $ \s -> OpenParen }
  ")"			{ pos $ \s -> CloseParen }

  "+"			{ pos $ \s -> Plus }
  "-"			{ pos $ \s -> Dash }
  "*"			{ pos $ \s -> Asterisk }
  "/"			{ pos $ \s -> Slash }
  "^"			{ pos $ \s -> Circ }
  ","			{ pos $ \s -> Comma }
  "="			{ pos $ \s -> Eq }
  "=="			{ pos $ \s -> Eq2 }
  ":"			{ pos $ \s -> Colon }

  -- This needs to be extended to multiline strings
  \"[^\"]*\"		{ pos $ \s -> StringLit (qstrip $ s) }

  $digit+		{ pos $ \s -> IntLit (read s) }
  @ident		{ pos $ \s -> ident_or_keyword s }
  .			{ pos $ \s -> error $ "unexpected: " ++ s }

{

data Token = Tok Sym AlexPosn
  deriving (Show)

pos f = \p -> \s -> Tok (f s) p

data Sym =
  Fun | Var | Const |
  External | Struct |

  Int | Bool | Float | Bytes |
  Ident String |

  Plus | Dash | Asterisk | Slash | Circ |
  Eq | Eq2 |

  Type AST.Type |

  OpenParen | CloseParen |
  Comma | Colon |

  IntLit Int | StringLit String
  deriving (Show)

qstrip = tail . init

keywords = [ ("fun", Fun),
	     ("var", Var),
	     ("const", Const),
	     ("external", External),
	     ("struct", Struct)
	   ]

ident_or_keyword s = case lookup s keywords of
		       Just t -> t
		       Nothing -> ident_or_type s

ident_or_type s = case lookup s AST.cmtTypeTable of
		    Just t -> Type t
		    Nothing -> Ident s

}
