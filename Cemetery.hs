module Main where

import Control.Monad.Reader
import Data.Either
import System.Console.GetOpt
import System.Environment
import System.Exit
import AST
import Lexer

data Opts = StopLexer | StopParse | Verbose deriving (Eq, Show)

-- Monadic type for the program logic
type App = ReaderT ([Opts], String) IO

options = [
 Option [] ["parse", "ast"] (NoArg StopParse) "stop at parsing stage",
 Option [] ["lexer", "lex", "toks"] (NoArg StopLexer) "stop at lexing stage",
 Option ['V'] ["verbose"] (NoArg Verbose) "be more verbose"
 ]

main = do args <- getArgs
          let (flags, nonOpts, msgs) = getOpt Permute options args

          if nonOpts == [] then
            do putStr (concat msgs ++ usageInfo "fatal: no input files" options)
               exitFailure
          else
            return ()

          let filename = head nonOpts

          if msgs /= [] || nonOpts == [] then
            do putStr (concat msgs ++ usageInfo "" options)
               exitFailure
          else return ()

          mapM (\f -> runReaderT work (flags, f)) (map base nonOpts)

breakIf f = do (opts, _) <- ask
               if elem f opts then
                 lift exitSuccess
               else
                 return ()

dbg s = do lift (putStr s)
dbgLn s = do lift (putStrLn s)

base :: String -> String
base s = if drop (length s - 4) s == ".cmt"
         then take (length s - 4) s
         else error "unrecognized file type"

get_toks = do c <- alexMonadScan
              case (c, last c) of
                ([],_) -> get_toks
                (_, EOF) -> return c
                _ -> do cs <- get_toks
                        return (c ++ cs)

work :: App ()
work = do (opts, basename) <- ask
          let inp  = basename ++ ".cmt"
          let outC = basename ++ ".c"
          let outH = basename ++ ".h"

          source <- lift $ readFile inp

          let res = runAlex source get_toks

          let toks = case res of
                       Left e -> error e
                       Right t -> t

          dbg "Tokens:"
          dbgLn $ concat $ map ((\s -> s ++ "\n").show) toks

          breakIf StopLexer

          return ()
