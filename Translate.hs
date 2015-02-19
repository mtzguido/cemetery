module Translate where

import CLang as C
import AST as A
import Control.Monad.Identity

type TranslateMonad = Identity
type TM = TranslateMonad -- Only for brevity

translate :: A.Prog -> TM C.Prog
translate (gd, fd) = do dd <- mapM translate_decls gd
                        ff <- mapM translate_funs fd
                        return $ concat dd ++ concat ff

translate_decls :: A.Decl -> TM [C.Unit]
translate_decls d = do return [UnitStub]

translate_funs :: A.FunDecl -> TM [C.Unit]
translate_funs f = do return [UnitStub]

runTranslate :: TM a -> a
runTranslate = runIdentity

semanticT :: A.Prog -> C.Prog
semanticT = runTranslate.translate
