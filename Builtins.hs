-- Cemetery built-in functions.

module Builtins where

import AST
import TMonad

e :: EnvV
e = EnvV { typ = Invalid, ir_name = "" }

builtins = [
 ("even", e { typ = Fun [Int] Bool, ir_name = "__cmt_even"})
 ]
