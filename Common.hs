module Common where

import Data.List (elemIndex)
import Control.Monad.Error
import Debug.Trace

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to seq = map repl1 seq where
                      repl1 x = case elemIndex x from of
                                     Nothing -> x
                                     Just i -> to !! i

traceM :: (Monad m) => String -> m ()
traceM s = trace s (return ())

-- Single stub error, should be expanded later on
data CmtError = CmtErr
  deriving Show

instance Error CmtError where
