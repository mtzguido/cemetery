module Parser where

import AST
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

-- Helpers
trySeq :: [Parser x] -> Parser x
trySeq s = foldl1 (<|>) (map try s)

cmtStyle :: LanguageDef st
cmtStyle = emptyDef {
             commentStart = "{-",
             commentEnd = "-}",
             commentLine = "--",
             nestedComments = True,
             identStart = letter,
             identLetter = alphaNum <|> char '_',
             opStart = oneOf "+-/*~&|^<>=;",
             opLetter = oneOf "+-/*~&|^<>=;",
             reservedNames = [ "fun", "var", "if", "then", "else",
                               "for", "while", "foreach", "nil", "empty"
                               ],
             reservedOpNames = [ "{", "}", ",", "=" ],
             caseSensitive = True
           }

tokenizer :: TokenParser u
tokenizer = makeTokenParser cmtStyle

cmtIdentifier = identifier tokenizer
cmtReserved = reserved tokenizer
cmtReservedOp = reservedOp tokenizer
cmtParens = parens tokenizer
cmtInteger = integer tokenizer

cmtProg :: Parser Prog
cmtProg = do r <- sepBy cmtFun (many space) -- Only function definitions for now
             eof
             return r

cmtFun :: Parser FunDecl
cmtFun = do cmtReserved "fun"
            name <- cmtIdentifier
            args <- cmtParens (sepBy cmtArgSpec (cmtReservedOp ","))
            cmtReservedOp ":"
            ret_type <- cmtType
            cmtReservedOp "{"
            body <- cmtStmts
            cmtReservedOp "}"
            return (FunDecl { name = name, ret = ret_type,
                              args = concat args, body = body})

cmtArgSpec :: Parser [(String, Type)]
cmtArgSpec = do i <- many cmtIdentifier
                cmtReservedOp ":"
                t <- cmtType
                return (map (\n -> (n,t)) i)

cmtStmts :: Parser Stmt
cmtStmts = do l <- many cmtStmt
              let r = case l of
                        [] -> Skip
                        _  -> foldr1 Seq l
              return r

cmtAtom :: Parser Expr
cmtAtom =     try cmtExprInt
          <|> cmtExprUnOp
          <|> cmtExprVar
          <|> (cmtParens cmtExpr)

cmtExpr :: Parser Expr
cmtExpr = do a <- cmtAtom
             do o <- cmtBinOp
                e <- cmtExpr
                return (BinOp o a e)
              <|> return a

cmtUnOp :: Parser UnOp
cmtUnOp = trySeq (map ff cmtUnOpTable) where
              ff (s, f) = do cmtReservedOp s
                             return f

cmtBinOp :: Parser BinOp
cmtBinOp = trySeq (map ff cmtBinOpTable) where
               ff (s, f) = do cmtReservedOp s
                              return f

cmtExprUnOp :: Parser Expr
cmtExprUnOp = do o <- cmtUnOp
                 e <- cmtExpr
                 return $ UnOp o e

cmtExprVar :: Parser Expr
cmtExprVar = do n <- cmtIdentifier
                do args <- cmtParens $ sepBy cmtExpr (cmtReservedOp ",")
                   return $ Call n args
                 <|> (return $ Var n)

cmtExprInt :: Parser Expr
cmtExprInt = do i <- cmtInteger
                let ii = fromIntegral i
                return (ConstInt ii)

cmtAssignment :: Parser Stmt
cmtAssignment = do v <- cmtIdentifier
                   cmtReservedOp "="
                   e <- cmtExpr
                   return (Assign v e)

cmtReturn :: Parser Stmt
cmtReturn = do cmtReserved "return"
               e <- cmtExpr
               return (Return e)

cmtDeclare :: Parser Stmt
cmtDeclare = do cmtReserved "declare"
                s <- cmtIdentifier
                return (Declare s)

cmtStmt :: Parser Stmt
cmtStmt = do s <- trySeq [cmtAssignment, cmtReturn, cmtDeclare]
             cmtReservedOp ";"
             return s
          <|> cmtBlock <|> cmtIf

cmtBlock :: Parser Stmt
cmtBlock = do cmtReservedOp "{"
              s <- cmtStmt
              cmtReservedOp "}"
              return s

cmtIf :: Parser Stmt
cmtIf = do cmtReserved "if"
           cond <- cmtParens cmtExpr
           th <- cmtStmt
           do cmtReserved "else"
              el <- cmtStmt
              return (If cond th el)
            <|> return (If cond th Skip)


typeLookup :: String -> Maybe Type
typeLookup s = lookup s cmtTypeTable

cmtType :: Parser Type
cmtType = try $ do name <- cmtIdentifier
                   let t = typeLookup name
                   case t of
                      Just tt -> return tt
                      Nothing -> error ("unknown type: " ++ name)
