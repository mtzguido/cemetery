-- Cemetery optimization module
--
-- This will, in the long run, include rules to optimize the
-- intermediate representation so we can generate a better source code
-- without depending on the output language.
--
-- We could later implement language-specific optimization if needed.
--
-- Do nothing for now.

module Optimize where

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity

import IR

data OMState =
  OMState {
    -- Counter to get fresh temp variables
    counter :: Int
  }

init_state = OMState { counter = 0 }

type OM = StateT OMState Identity

runOM :: OM a -> a
runOM m = let m' = runStateT m init_state
              (a, _s) = runIdentity m'
           in a

optimize :: IR -> IR
optimize p = map (runOM.o_unit) p

o_unit (FunDef ft b) =
    do b' <- o_body b
       return $ FunDef ft b'

o_unit (Decl d) = -- do nothing with declarations
    do return $ Decl d

o_body b =
    do return b

