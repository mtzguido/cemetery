-- Cemetery built-in functions.

module Builtins where

import AST

builtins = [
 ("even", Fun [Int] Int, "__cmt_even")
 ]
