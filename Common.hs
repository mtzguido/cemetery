module Common where

import Data.List (elemIndex)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to seq = map repl1 seq where
                      repl1 x = case elemIndex x from of
                                     Nothing -> x
                                     Just i -> to !! i
