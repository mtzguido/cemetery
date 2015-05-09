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
import CGen
import Common

data Opts = StopLexer | StopParse | StopTranslate
          | Verbose deriving (Eq, Show)

-- Monadic type for the program logic
type App = ReaderT ([Opts], String) IO

options = [
 Option [] ["lexer", "lex", "toks"] (NoArg StopLexer) "stop after lexing stage",
 Option [] ["parse", "ast"] (NoArg StopParse) "stop after parsing stage",
 Option [] ["translate", "trans"] (NoArg StopTranslate) "stop after translation",
 Option ['v'] ["verbose"] (NoArg Verbose) "be more verbose"
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

dbg s   = do (v, _) <- ask
             when (elem Verbose v) $ lift (putStr s)
dbgLn s = do (v, _) <- ask
             when (elem Verbose v) $ lift (putStrLn s)

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

hprologue = ""
cprologue =
  unlines ["#include <stdbool.h>",
           "#include <stdint.h>",
           "",
           "typedef uint8_t cmt_buf_t[];"]

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

          case ast of
            [] -> error "File is empty"
            _  -> return ()

          dbg "AST: "
          dbgLn $ show ast
          dbgLn ""

          breakIf StopParse

          let (st, cast) = semanticT ast
          dbgLn $ "Translation final state: " ++ show st
          dbgLn ""

          case cast of
            Left e -> do dbg $ "ERROR: " ++ show e
                         lift exitFailure
            Right t -> do dbg "C AST: "
                          dbgLn $ show t

          let Right t = cast
          dbgLn ""

          breakIf StopTranslate

          let ctext = cgenT t
          let htext = ""
          dbgLn $ "ctext = " ++ ctext

          lift $ writeFile outC (cprologue ++ ctext)
          lift $ writeFile outH (hprologue ++ htext)

          return ()
