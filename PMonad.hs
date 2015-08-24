module PMonad where

import Common
import Control.Monad.Error
import Control.Monad.Identity

type PMonad = ErrorT CmtError Identity

runPMonad m = runIdentity (runErrorT m)
