module PMonad where

import Common
import Control.Monad.Except
import Control.Monad.Identity

type PMonad = ExceptT CmtError Identity

runPMonad m = runIdentity (runExceptT m)
