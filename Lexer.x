{
module Lexer (Token(..), Sym(..), alexMonadScan, runAlex, alexEOF) where

import qualified AST
import Debug.Trace
import Control.Monad
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$hex = [$digit a-fA-F]
$alnum_ = [$alpha '_' $digit]
$backslash = \
@ident = $alpha $alnum_*

tokens :-
<comm>		"-}"	{ popComm }
<comm>		"{-"	{ pushComm }
<comm>		.	;

<0> {
  $white+		;
  "--".*		;
  $backslash $		{ backslash }

  "{-"			{ pushComm }

  "("			{ simple Paren }
  ")"			{ simple Unparen }
  "{"			{ simple Brace }
  "}"			{ simple Unbrace }
  "["			{ simple Square }
  "]"			{ simple Unsquare }
  "<"			{ simple Langle }
  ">"			{ simple Rangle }
  "<="			{ simple Le }
  ">="			{ simple Ge }

  "<<"			{ simple LShift }
  ">>"			{ simple RShift }
  "<<<"			{ simple LRot }
  ">>>"			{ simple RRot }


  "+"			{ simple Plus }
  "-"			{ simple Dash }
  "*"			{ simple Asterisk }
  "/"			{ simple Slash }
  "^"			{ simple Circ }
  "%"			{ simple Perc }
  "&"			{ simple Amp }
  "|"			{ simple Pipe }
  "~"			{ simple Tilde }
  "|||"			{ simple Concat }

  "&&"			{ simple And }
  "||"			{ simple Or }
  "!"			{ simple Not }

  ","			{ simple Comma }
  "="			{ simple Eq }
  "=="			{ simple Eq2 }
  "!="			{ simple Ne }
  ":"			{ simple Colon }
  ";"			{ simple Break }
  "."			{ simple Dot }
  "::"			{ simple Colon2 }
  ".."			{ simple Dot2 }

  "+="			{ simple PlusAssign }
  "-="			{ simple MinusAssign }
  "*="			{ simple ProdAssign }
  "/="			{ simple DivAssign }
  "^="			{ simple XorAssign }

  "false"		{ simple (BoolLit False) }
  "true"		{ simple (BoolLit True) }

  -- This needs to be extended to multiline strings
  \"[^\"]*\"		{ ind $ stringLit }

  $digit+ \. $digit+	{ ind $ floatLit }
  $digit+		{ ind $ intLit }
  0x($hex*)		{ ind $ intLit }

  @ident		{ ind $ ident_or_keyword' }
  .			{ ind $ \(_,_,_,s) -> error $ "unexpected: " ++ s }
}

{

pushComm :: AlexAction [Token]
pushComm p i = do incrCommLevel
                  begin comm p i

popComm :: AlexAction [Token]
popComm p i = do decrCommLevel
                 n <- getCommLevel
                 if n < 0 then alexError "parens" else return ()
                 if n == 0
                   then begin 0 p i
                   else return []

fakePos = AlexPn 0 0 0

data AlexUserState = CmtState { levels :: [Int], curr :: Int,
                                commentLevel :: Int } deriving Show

incrCommLevel:: Alex ()
incrCommLevel = do s <- alexGetUserState
                   let ss = s { commentLevel = commentLevel s + 1 }
                   alexSetUserState ss

decrCommLevel:: Alex ()
decrCommLevel = do s <- alexGetUserState
                   let ss = s { commentLevel = commentLevel s - 1 }
                   alexSetUserState ss

getCommLevel:: Alex Int
getCommLevel = do s <- alexGetUserState
                  return (commentLevel s)

alexInitUserState = CmtState { levels = [1], curr = -1, commentLevel = 0 }

getIndentInfo = do s <- alexGetUserState
                   return (levels s, curr s)

setIndentInfo i = do s <- alexGetUserState
                     alexSetUserState $ s { levels = fst i, curr = snd i }

-- Revisit.
alexEOF = do (inds, ll) <- getIndentInfo
             break <- fake Break
             unbrace <- fake Unbrace
             if 1 == head inds then
               do return [break, EOF]
             else
               do let (npop, newinds) = nlevels 1 inds
                  return $ [break]
                        ++ (replicate npop unbrace)
                        ++ [EOF]

nlevels c inds = let (l,r) = break (<=c) inds
                 in (length l, r)

fake :: Sym -> Alex Token
fake t = do (p,_,_,_) <- alexGetInput
            return $ Tok t p

ind :: AlexAction Token -> AlexAction [Token]
ind m ai@(p,_,_,s) l = do t <- m ai l
                          let AlexPn _ l c = p
                          (inds, ll) <- getIndentInfo
                          break <- fake Break
                          brace <- fake Brace
                          unbrace <- fake Unbrace
                          if l < ll then
                            error "internal error 1"
                          else if l == ll || inds == [] then
                            return [t]
                          else -- l > ll
                            do setIndentInfo (inds, l) -- Update the line
                               if ll == -1 then
                                 return [t]
                               else if c > head inds then
                                 do setIndentInfo (c:inds, l)
                                    return [brace, t]
                               else if c == head inds then
                                 do return [break, t]
                               else
                                 do let (npop, newinds) = nlevels c inds
                                    setIndentInfo (newinds, l)
                                    return $ [break]
                                          ++ (replicate npop unbrace)
                                          ++ [t]

backslash (p,_,_,_) _ = do (inds, ll) <- getIndentInfo
                           setIndentInfo (inds, ll+1)
                           return []

data Token = Tok Sym AlexPosn | EOF
  deriving (Show)

simple = ind.simple'
simple' v (p,_,_,s) i = do return $ Tok v p

stringLit (p,_,_,s) l = return $ Tok (StringLit (qstrip (take l s))) p
intLit    (p,_,_,s) l = return $ Tok (IntLit (read (take l s) :: Int)) p
floatLit  (p,_,_,s) l = return $ Tok (FloatLit (read (take l s) :: Double)) p

data Sym =
  Fun | Var | Const |
  Extern |

  Return | If | Else | For | In |

  Int | Bool | Float | Bits |
  Ident String |


  Plus | Dash | Asterisk | Slash | Circ | Perc |
  Amp | Pipe | Tilde | Concat |
  Ne | Eq | Eq2 | Dot | Dot2 |

  Type AST.Type |

  Backslash |

  Paren | Unparen |
  Square | Unsquare |
  Brace | Unbrace | Break |
  Langle | Rangle | Le | Ge |
  LShift | RShift | LRot | RRot |
  Comma | Colon | Colon2 |

  PlusAssign | MinusAssign |
  ProdAssign | DivAssign |
  XorAssign |

  And | Or | Not |

  IntLit Int | StringLit String | FloatLit Double |
  BoolLit Bool |

  Error
  deriving (Show)

qstrip = tail . init

keywords = [ ("fun", Fun),
             ("var", Var),
             ("const", Const),
             ("external", Extern),
             ("return", Return),
             ("if", If),
             ("else", Else),
             ("for", For),
             ("in", In),
             ("error", Error)
           ]

ident_or_keyword' (p,_,_,s) l = return $ Tok (ident_or_keyword (take l s)) p

ident_or_keyword s = case lookup s keywords of
                       Just t -> t
                       Nothing -> ident_or_type s

ident_or_type s = case lookup s AST.cmtTypeTable of
                    Just t -> Type t
                    Nothing -> Ident s
}
