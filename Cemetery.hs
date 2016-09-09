module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Either
import Data.List
import System.Console.GetOpt as Opt
import System.Environment
import System.Exit
import Text.Regex

import AST
import CGen
import Common
import CPrint
import IR as I
import Lexer
import Optimize
import Parser
import PMonad
import Prologue
import TMonad
import Translate

data Opts = StopLexer
          | StopParse
          | StopTranslate
          | StopGen
          | NoOutput
          | Verbose
          | NoOptimize
    deriving (Eq, Show)

-- Monadic type for the program logic
type App = ReaderT ([Opts], String) (
            ExceptT CmtError (
             IO
           ))

runApp m r = runExceptT (runReaderT m r)

options = [
 Option ['n'] ["no-output"]         (NoArg NoOutput)        "don't output to files",
 Option []    ["lexer", "toks"]     (NoArg StopLexer)       "stop after lexing stage",
 Option []    ["parse", "ast"]      (NoArg StopParse)       "stop after parsing stage",
 Option []    ["translate", "ir"]   (NoArg StopTranslate)   "stop after translation",
 Option []    ["generate"]          (NoArg StopGen)         "stop after genering a C ast",
 Option ['v'] ["verbose"]           (NoArg Verbose)         "be more verbose",
 Option []    ["no-optimize"]       (NoArg NoOptimize)      "don't optimize the IR"
 ]

main =
    do args <- getArgs
       let (flags, nonOpts, msgs) = getOpt Opt.Permute options args

       when (nonOpts == []) $
         do putStr (concat msgs ++ usageInfo "fatal: no input files" options)
            exitFailure

       when (length nonOpts > 1) $
         do putStr (concat msgs ++ usageInfo "fatal: more than one input file" options)
            exitFailure

       let filename = head nonOpts

       if msgs /= [] || nonOpts == [] then
         do putStr (concat msgs ++ usageInfo "" options)
            exitFailure
       else return ()

       res <- runApp work (flags, filename)
       case res of
           Left e -> do putStrLn $ "An error ocurred: " ++ show e
                        exitFailure
           Right _ -> do liftIO exitSuccess

haveOpt :: Opts -> App Bool
haveOpt f =
    do (opts, _) <- ask
       return $ elem f opts

ifNotOpt :: Opts -> App () -> App ()
ifNotOpt f m =
    do (opts, _) <- ask
       when (not $ elem f opts) m

ifOpt :: Opts -> App () -> App ()
ifOpt f m =
    do (opts, _) <- ask
       when (elem f opts) m

breakIf :: Opts -> App ()
breakIf f =
    ifOpt f (liftIO exitSuccess)

dbg :: String -> App ()
dbg s =
    do (v, _) <- ask
       when (elem Verbose v) $ liftIO (putStr s)

dbgLn :: String -> App ()
dbgLn s =
    do (v, _) <- ask
       when (elem Verbose v) $ liftIO (putStrLn s)

base :: String -> App String
base s =
    do if drop (length s - 4) s == ".cmt"
           then return $ take (length s - 4) s
           else throwError $ CmtErr "unrecognized file type"

get_toks =
    do c <- alexMonadScan
       case (c, last c) of
         ([],_) -> get_toks
         (_, EOF) -> return c
         _ -> do cs <- get_toks
                 return (c ++ cs)

showIRUnit :: Unit -> App ()
showIRUnit ir =
    do dbgLn $ show ir

inc_regex = mkRegex "^include ([^ ]*) *$"

check_include :: String -> String -> App String
check_include b s =
    do case matchRegex inc_regex s of
           Nothing -> return s
           Just [x] -> cmtReadFile (b ++ x)
           _ -> throwError $ CmtErr "wat?"

dirname s = case elemIndices '/' s of
              [] -> "./"
              l -> take (1 + last l) s

cmtReadFile fname =
    do t <- liftIO $ readFile fname
       ls <- mapM (check_include (dirname fname)) (lines t)
       return (unlines ls)

print_parse_err text ((Tok Break (AlexPn _ _ _)):ts) =
    print_parse_err text ts

print_parse_err text ((Tok s (AlexPn _ l c)):ts) =
    do liftIO (putStrLn $ "Parse error near line " ++ show l)
       let ls = lines text
       liftIO (putStrLn $ "source> " ++ (ls !! (l-1)))
       liftIO (putStrLn $ "source> " ++ (replicate (c-1) ' ') ++ "^")
       return ()

print_parse_err text [EOF] =
    do liftIO (putStrLn $ "Parse error: unexpected end of file")
       let ls = lines text
       liftIO (putStrLn $ "source> " ++ (last ls))
       return ()

work :: App ()
work = do (opts, filename) <- ask
          stem <- base filename
          let inp  = stem ++ ".cmt"
          let outC = stem ++ ".c"
          let outH = stem ++ ".h"

          source <- cmtReadFile inp
          dbgLn $ "Actual source: " ++ source

          let res = runAlex source get_toks

          let toks = case res of
                       Left e -> error e
                       Right t -> t

          dbg "Tokens: "
          dbgLn $ concat $ map ((\s -> s ++ "\n").show) toks
          breakIf StopLexer

          let m = runPMonad $ cmtParse toks
          ast <- case m of
                     Left e@(ParseErr t) -> do print_parse_err source t
                                               throwError e
                     Left e ->  throwError e
                     Right a -> return a

          when (ast == []) $ throwError (CmtErr "File is empty")

          dbg "AST: "
          dbgLn $ show ast
          dbgLn ""

          breakIf StopParse

          let r = runTranslate (translate ast)
          (ir, st) <- case r of
                          Left e -> throwError e
                          Right x -> return x

          dbgLn $ "Translation final state: " ++ show st
          dbgLn ""

          dbgLn "IR Tree: "
          mapM showIRUnit ir
          dbgLn ""

          breakIf StopTranslate

          should_opt <- liftM not (haveOpt NoOptimize)
          oir <- if should_opt
                 then case runOM (optimize ir) of
                        Left e -> throwError e
                        Right (x,_) -> return x
                 else return ir

          dbgLn "Optimized IR: "
          mapM showIRUnit oir

          let (cast, hast) = cgen oir

          dbg "C AST:"
          dbgLn (show cast)
          dbg "H AST:"
          dbgLn (show hast)

          breakIf StopGen

          let ctext = cprint cprologue cast
          dbgLn "C Text:"
          dbgLn ctext

          let htext = incl_guard stem $ cprint hprologue hast
          dbgLn "H Text:"
          dbgLn htext

          ifNotOpt NoOutput $ liftIO $ writeFile outC ctext
          ifNotOpt NoOutput $ liftIO $ writeFile outH htext

          return ()

incl_guard i s = unlines $ ["#ifndef __CEMETERY_" ++ ii ++ "__",
                            "#define __CEMETERY_" ++ ii ++ "__", ""] ++
                           (lines s) ++
                           ["", "#endif"] where
                 ii = macro_sanitize i

macro_sanitize = tr "-/" (repeat '_')
