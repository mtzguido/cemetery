module Common where

import Data.List (elemIndex)
import Control.Monad.Error
import Lexer
import Debug.Trace

-- Works similarly to the Unix 'tr' command
tr :: Eq a => [a] -> [a] -> [a] -> [a]
tr from to seq = map repl1 seq where
                 repl1 x = case elemIndex x from of
                                Nothing -> x
                                Just i -> to !! i

traceM :: (Monad m) => String -> m ()
traceM s = trace s (return ())

data CmtError = CmtErr String  -- generic error
              | ParseErr [Token]

instance Show CmtError where
  show (CmtErr s) = "generic error: " ++ s
  show (ParseErr _) = "parse error"

shuf l r = concat $ map (\(a,b) -> [a,b]) $ zip l r

instance Error CmtError where
