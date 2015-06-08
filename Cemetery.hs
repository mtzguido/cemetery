module Main where

import Control.Monad.Reader
import Data.Either
import System.Console.GetOpt
import System.Environment
import System.Exit
import AST
import IR
import Lexer
import Parser
import Translate
import Common
import Prologue
import Optimize
import CGen
import CPrint

data Opts = StopLexer | StopParse | StopTranslate
          | StopGen
          | Verbose deriving (Eq, Show)

-- Monadic type for the program logic
type App = ReaderT ([Opts], String) IO

options = [
 Option [] ["lexer", "lex", "toks"] (NoArg StopLexer) "stop after lexing stage",
 Option [] ["parse", "ast"] (NoArg StopParse) "stop after parsing stage",
 Option [] ["translate", "trans"] (NoArg StopTranslate) "stop after translation",
 Option [] ["generate"] (NoArg StopGen) "stop after genering a C ast",
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

showIRUnit :: IR.Unit -> App ()
showIRUnit ir =
    do lift $ putStrLn $ show ir

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

          let (st, ir) = semanticT ast
          dbgLn $ "Translation final state: " ++ show st
          dbgLn ""

          case ir of
            Left e -> do dbg $ "ERROR: " ++ show e
                         lift exitFailure
            Right t -> do lift $ putStrLn "IR Tree: "
                          mapM showIRUnit t

          let Right ir' = ir

          breakIf StopTranslate

          let oir = optimize ir'

          lift $ putStrLn "Optimized IR: "
          mapM showIRUnit oir

          let cast = cgen oir

          lift $ putStrLn "C ast:"
          lift $ putStrLn (show cast)

          breakIf StopGen

          let ctext = cprint cast
          lift $ putStrLn "C text:"
          lift $ putStrLn ctext

          lift $ writeFile outC (cprologue ++ ctext)

          return ()
