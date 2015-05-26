module CGen where

import Data.List
import Data.Monoid
import qualified IR as I
import qualified CLang as C
import Common

import Control.Monad.Writer
import Control.Monad.Identity

cgen :: I.IR -> C.Prog
cgen p = C.Prog { C.includes = [], C.units = [] }

type GM = WriterT [C.Unit] (
           Identity
          )

g_ir :: I.IR -> GM ()
g_ir p = do bs <- mapM g_unit p
            return ()

g_unit :: I.Unit -> GM ()
g_unit (I.FunDef (I.Funtype { I.name = name,
                              I.args = args,
                              I.ret  = ret}) body) =
    do c_args <- g_args args
       c_ret <- g_type ret
       c_body <- g_body body
       let ft = C.Funtype { C.name = name, C.args = c_args, C.ret = c_ret }
       tell [C.FunDef ft c_body]

g_unit (I.UnitScaf) =
    do tell [C.Comment "UnitScaf"]

g_args :: [(String, I.Type)] -> GM [(String, C.Type)]
g_args _ = do return []

g_body :: I.Stmt -> GM C.Block
g_body b = do s <- g_stmt b
              return ([], s)

g_stmt :: I.Stmt -> GM C.Stmt
g_stmt _ =
    do return (C.Skip)

g_type _ = do return C.Int
