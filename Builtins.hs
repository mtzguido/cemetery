-- Cemetery built-in functions.

module Builtins where

import AST
import TMonad
import qualified IR as I

e = envv

builtins = [
 ("even",            e { typ = Fun [Int] Bool, ir_lv = I.LVar "__cmt_even"}),
 ("permute",         e { typ = Fun [Bits, ArrT Int] Bits, ir_lv = I.LVar "__cmt_permute"}),
 ("permute_inverse", e { typ = Fun [Bits, ArrT Int] Bits, ir_lv = I.LVar "__cmt_permute_inv"}),
 ("length",          e { typ = Fun [Bits] Int, ir_lv = I.LVar "__cmt_length"}),
 ("toint",           e { typ = Fun [Bits] Int, ir_lv = I.LVar "__cmt_toint"}),
 ("tobits",          e { typ = Fun [Int, Int] Bits, ir_lv = I.LVar "__cmt_tobits"}),
 ("zero",            e { typ = Fun [Int] Bits, ir_lv = I.LVar "__cmt_zero"})
 ]
