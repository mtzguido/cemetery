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
import Control.Monad.Error
import Control.Monad.Identity

import Common
import IR

data OMState =
  OMState {
    -- Counter to get fresh temp variables
    counter :: Int
  }

init_state = OMState { counter = 0 }

type OM = ErrorT CmtError (
           StateT OMState (
            Identity
          ))

runOM m = runIdentity $ runStateT (runErrorT m) init_state

optimize :: IR -> OM IR
optimize p = mapM o_unit p

o_unit (FunDef ft b) =
    do let (_, s) = b
       c <- check_return_paths s
       when (not c) $ throwError $
           CmtErr ("Some paths in '" ++ name ft ++ "' do not return!")
       b' <- o_body b
       return $ FunDef ft b'

o_unit (Decl d) = -- do nothing with declarations
    do return $ Decl d

o_body b =
    do return b

check_return_paths (Return _) =
    do return True
check_return_paths (Error _) =
    do return True
check_return_paths (Seq l r) =
    do lp <- check_return_paths l
       rp <- check_return_paths r
       return $ lp || rp
check_return_paths (If _ (_,t) (_,e)) =
    do tp <- check_return_paths t
       ep <- check_return_paths e
       return $ tp && ep
check_return_paths (For _ _ _ (_,b)) =
    do check_return_paths b
check_return_paths _ =
    do return False
