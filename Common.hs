module Common where

import Data.List (elemIndex)
import Control.Monad.Error
import Debug.Trace

-- Works similarly to the Unix 'tr' command
tr :: Eq a => [a] -> [a] -> [a] -> [a]
tr from to seq = map repl1 seq where
                 repl1 x = case elemIndex x from of
                                Nothing -> x
                                Just i -> to !! i

traceM :: (Monad m) => String -> m ()
traceM s = trace s (return ())

-- Indent every line with one tab
indent :: String -> String
indent s = unlines $ map ('\t':) $ lines s

-- Suposedly in package Maybes, but let's not introduce
-- dependencies
fmapM_maybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
fmapM_maybe f Nothing  = do return Nothing
fmapM_maybe f (Just a) = do v <- f a
                            return (Just v)

-- Single stub error, should be expanded later on
data CmtError = CmtErr String
  deriving Show

instance Error CmtError where
