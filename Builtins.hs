-- Cemetery built-in functions.

module Builtins where

import AST
import TMonad

e = envv

builtins = [
 ("even", e { typ = Fun [Int] Bool, ir_name = "__cmt_even"}),
 ("permute", e { typ = Fun [Bits, ArrT Int] Bits, ir_name = "__cmt_permute"}),
 ("permute_inverse", e { typ = Fun [Bits, ArrT Int] Bits, ir_name = "__cmt_permute_inv"}),
 ("toint", e { typ = Fun [Bits] Int, ir_name = "__cmt_toint"}),
 ("tobits", e { typ = Fun [Int] Bits, ir_name = "__cmt_tobits"})
 ]
