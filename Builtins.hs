-- Cemetery built-in functions.

module Builtins where

import AST
import TMonad
import qualified IR as I

e = eseman
b = I.LV . I.Builtin

builtins = [
 ("permute",     e { typ = Fun [Bits, ArrT Int Nothing, Int] Bits,
                     expr = b I.Permute }),
 ("length",      e { typ = Fun [Bits] Int,        expr = b I.Length }),
 ("toint",       e { typ = Fun [Bits] Int,        expr = b I.ToInt }),
 ("tobits",      e { typ = Fun [Int, Int] Bits,   expr = b I.ToBits }),
 ("zero",        e { typ = Fun [Int] Bits,        expr = b I.Zero })
 ]
