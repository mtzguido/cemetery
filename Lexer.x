{
module Lexer (Token(..), Sym(..), alexMonadScan, runAlex) where

import qualified AST
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum_ = [$alpha '_' $digit]
@ident = $alpha $alnum_*

tokens :-
  $white+		;
  "--".*		;

  "("			{ simple OpenParen }
  ")"			{ simple CloseParen }

  "+"			{ simple Plus }
  "-"			{ simple Dash }
  "*"			{ simple Asterisk }
  "/"			{ simple Slash }
  "^"			{ simple Circ }
  ","			{ simple Comma }
  "="			{ simple Eq }
  "=="			{ simple Eq2 }
  ":"			{ simple Colon }

  -- This needs to be extended to multiline strings
  \"[^\"]*\"		{ stringLit }

  $digit+		{ intLit }
  @ident		{ ident_or_keyword' }
  .			{ \(_,_,_,s) -> error $ "unexpected: " ++ s }

{

alexEOF = return EOF

data Token = Tok Sym AlexPosn | EOF
  deriving (Show)

simple v = \(p,_,_,s) -> \i -> return $ Tok v p

stringLit (p,_,_,s) l = return $ Tok (StringLit (qstrip (take l s))) p
intLit (p,_,_,s) l = return $ Tok (IntLit (read (take l s) :: Int)) p

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

ident_or_keyword' (p,_,_,s) l = return $ Tok (ident_or_keyword (take l s)) p

ident_or_keyword s = case lookup s keywords of
		       Just t -> t
		       Nothing -> ident_or_type s

ident_or_type s = case lookup s AST.cmtTypeTable of
		    Just t -> Type t
		    Nothing -> Ident s
}
