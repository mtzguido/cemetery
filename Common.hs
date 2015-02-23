module Common where

import Data.List (elemIndex)
import Control.Monad.Error

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to seq = map repl1 seq where
                      repl1 x = case elemIndex x from of
                                     Nothing -> x
                                     Just i -> to !! i

-- Single stub error, should be expanded later on
data CmtError = CmtErr
  deriving Show

instance Error CmtError where
