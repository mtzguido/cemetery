{
module Lexer (Token(..), Sym(..), alexMonadScan, runAlex, alexEOF) where

import qualified AST
import Debug.Trace
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum_ = [$alpha '_' $digit]
$backslash = \
@ident = $alpha $alnum_*

tokens :-
  $white+		;
  "--".*		;
  $backslash $		{ backslash }

  "("			{ simple Paren }
  ")"			{ simple Unparen }
  "{"			{ simple Brace }
  "}"			{ simple Unbrace }
  "["			{ simple Square }
  "]"			{ simple Unsquare }


  "+"			{ simple Plus }
  "-"			{ simple Dash }
  "*"			{ simple Asterisk }
  "/"			{ simple Slash }
  "^"			{ simple Circ }
  "%"			{ simple Perc }

  ","			{ simple Comma }
  "="			{ simple Eq }
  "=="			{ simple Eq2 }
  ":"			{ simple Colon }
  ";"			{ simple Break }
  "."			{ simple Dot }

  -- This needs to be extended to multiline strings
  \"[^\"]*\"		{ ind $ stringLit }

  $digit+		{ ind $ intLit }
  @ident		{ ind $ ident_or_keyword' }
  .			{ ind $ \(_,_,_,s) -> error $ "unexpected: " ++ s }

{

fakePos = AlexPn 0 0 0

type AlexUserState = ([Int], Int)
alexInitUserState = ([1], 1)

alexEOF = do (inds, ll) <- alexGetUserState
             if 1 == head inds then
               do return [fake Break, EOF]
             else
               do let (npop, newinds) = nlevels 1 inds
                  return $ [fake Break]
                        ++ (replicate npop (fake Unbrace))
                        ++ [EOF]

nlevels c inds = let (l,r) = break (<=c) inds
                 in (length l, r)

fake :: Sym -> Token
fake t = Tok t fakePos

ind :: (AlexInput -> Int -> Alex Token) -> AlexInput -> Int -> Alex [Token]
ind m ai@(p,_,_,s) l = do t <- m ai l
                          let AlexPn _ l c = p
                          (inds, ll) <- alexGetUserState
                          if l < ll then
                            error "internal error 1"
                          else if l == ll || inds == [] then
                            return [t]
                          else -- l > ll
                            do alexSetUserState (inds, l) -- Update the line
                               if c > head inds then
                                 do alexSetUserState (c:inds, l)
                                    return [fake Brace, t]
                               else if c == head inds then
                                 do return [fake Break, t]
                               else
                                 do let (npop, newinds) = nlevels c inds
                                    alexSetUserState (newinds, l)
                                    return $ [fake Break]
                                          ++ (replicate npop (fake Unbrace))
                                          ++ [t]

backslash (p,_,_,_) _ = do (inds, ll) <- alexGetUserState
                           alexSetUserState (inds, ll+1)
                           return []

data Token = Tok Sym AlexPosn | EOF | NoTok
  deriving (Show)

simple = ind.simple'
simple' v (p,_,_,s) i = do return $ Tok v p

stringLit (p,_,_,s) l = return $ Tok (StringLit (qstrip (take l s))) p
intLit (p,_,_,s) l = return $ Tok (IntLit (read (take l s) :: Int)) p

data Sym =
  Fun | Var | Const |
  External | Struct |
  Declare |

  Return | If | Else |

  Int | Bool | Float | Bytes |
  Ident String |

  Plus | Dash | Asterisk | Slash | Circ | Perc |
  Eq | Eq2 | Dot |

  Type AST.Type |

  Backslash |

  Paren | Unparen |
  Square | Unsquare |
  Brace | Unbrace | Break |
  Comma | Colon |

  IntLit Int | StringLit String
  deriving (Show)

qstrip = tail . init

keywords = [ ("fun", Fun),
             ("var", Var),
             ("const", Const),
             ("external", External),
             ("struct", Struct),
             ("return", Return),
             ("if", If),
             ("else", Else),
             ("declare", Declare)
           ]

ident_or_keyword' (p,_,_,s) l = return $ Tok (ident_or_keyword (take l s)) p

ident_or_keyword s = case lookup s keywords of
                       Just t -> t
                       Nothing -> ident_or_type s

ident_or_type s = case lookup s AST.cmtTypeTable of
                    Just t -> Type t
                    Nothing -> Ident s
}
