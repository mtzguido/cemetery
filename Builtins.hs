-- Cemetery built-in functions.

module Builtins where

import AST

builtins = [
 ("even", Fun [Int] Bool, "__cmt_even")
 ]
