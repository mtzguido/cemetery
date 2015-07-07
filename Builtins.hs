-- Cemetery built-in functions.

module Builtins where

import AST
import TMonad
import qualified IR as I

e = envv

builtins = [
 ("permute",         e { typ = Fun [Bits, ArrT Int, Int] Bits, ir_lv = I.LVar "__cmt_permute"}),
 ("length",          e { typ = Fun [Bits] Int, ir_lv = I.LVar "__cmt_length"}),
 ("toint",           e { typ = Fun [Bits] Int, ir_lv = I.LVar "__cmt_toint"}),
 ("tobits",          e { typ = Fun [Int, Int] Bits, ir_lv = I.LVar "__cmt_tobits"}),
 ("zero",            e { typ = Fun [Int] Bits, ir_lv = I.LVar "__cmt_zero"})
 ]
