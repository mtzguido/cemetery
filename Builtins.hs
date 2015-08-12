-- Cemetery built-in functions.

module Builtins where

import AST
import TMonad
import qualified IR as I

e = envv
b = I.Builtin

builtins = [
 ("permute",     e { typ = Fun [Bits, ArrT Int Nothing, Int] Bits,
                     ir_lv = b I.Permute }),
 ("length",      e { typ = Fun [Bits] Int,                  ir_lv = b I.Length }),
 ("toint",       e { typ = Fun [Bits] Int,                  ir_lv = b I.ToInt }),
 ("tobits",      e { typ = Fun [Int, Int] Bits,             ir_lv = b I.ToBits }),
 ("zero",        e { typ = Fun [Int] Bits,                  ir_lv = b I.Zero })
 ]
