main = do pc <- readFile "Prologue.c"
          ph <- readFile "Prologue.h"
          let contents = "module Prologue where\n\n" ++
                          "cprologue = " ++ show pc ++ "\n" ++
                          "hprologue = " ++ show ph ++ "\n"
          writeFile "Prologue.hs" contents
