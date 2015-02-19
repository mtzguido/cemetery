module Main where

import Control.Monad.Reader
import Data.Either
import System.Console.GetOpt
import System.Environment
import System.Exit
import AST
import Lexer
import Parser
import Translate

data Opts = StopLexer | StopParse | StopTranslate
          | Verbose deriving (Eq, Show)

-- Monadic type for the program logic
type App = ReaderT ([Opts], String) IO

options = [
 Option [] ["lexer", "lex", "toks"] (NoArg StopLexer) "stop after lexing stage",
 Option [] ["parse", "ast"] (NoArg StopParse) "stop after parsing stage",
 Option [] ["translate", "trans"] (NoArg StopLexer) "stop after translation",
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

          mapM (do1Work flags) (map base nonOpts)

do1Work flags file = runReaderT work (flags, file)

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

          dbg "Tokens: "
          dbgLn $ concat $ map ((\s -> s ++ "\n").show) toks

          breakIf StopLexer

          let ast = cmtParse toks

          dbg "AST: "
          dbgLn $ show ast

          breakIf StopParse

          let cast = translate ast
          dbg "C AST: "
          dbgLn $ show cast

          breakIf StopTranslate

          return ()
