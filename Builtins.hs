-- Cemetery built-in functions.

module Builtins where

import AST
import TMonad

e = envv

builtins = [
 ("even", e { typ = Fun [Int] Bool, ir_name = "__cmt_even"})
 ]
