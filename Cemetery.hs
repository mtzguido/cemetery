module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import Data.Either
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.Regex

import AST
import CGen
import Common
import CPrint
import IR
import Lexer
import Optimize
import Parser
import Prologue
import TMonad
import Translate

data Opts = StopLexer
          | StopParse
          | StopTranslate
          | StopGen
          | NoOutput
          | Verbose
    deriving (Eq, Show)

-- Monadic type for the program logic
type App = ReaderT ([Opts], String) (
            ErrorT CmtError (
             IO
           ))

runApp m r = runErrorT (runReaderT m r)

options = [
 Option ['n'] ["no-output"]         (NoArg NoOutput)        "don't output to files",
 Option []    ["lexer", "toks"]     (NoArg StopLexer)       "stop after lexing stage",
 Option []    ["parse", "ast"]      (NoArg StopParse)       "stop after parsing stage",
 Option []    ["translate"]         (NoArg StopTranslate)   "stop after translation",
 Option []    ["generate"]          (NoArg StopGen)         "stop after genering a C ast",
 Option ['v'] ["verbose"]           (NoArg Verbose)         "be more verbose"
 ]

main =
    do args <- getArgs
       let (flags, nonOpts, msgs) = getOpt Permute options args

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

ifNotOpt f m =
    do (opts, _) <- ask
       when (not $ elem f opts) m

ifOpt f m =
    do (opts, _) <- ask
       when (elem f opts) m

breakIf f =
    ifOpt f (liftIO exitSuccess)

dbg s =
    do (v, _) <- ask
       when (elem Verbose v) $ liftIO (putStr s)

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

showIRUnit :: IR.Unit -> App ()
showIRUnit ir =
    do liftIO $ putStrLn $ show ir

inc_regex = mkRegex "^include ([^ ]*) *$"

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

          let ast = cmtParse toks
          when (ast == []) $ throwError (CmtErr "File is empty")

          dbg "AST: "
          dbgLn $ show ast
          dbgLn ""

          breakIf StopParse

          let (st, ir) = runTranslate (translate ast)

          dbgLn $ "Translation final state: " ++ show st
          dbgLn ""

          case ir of
            Left e -> do throwError e
            Right t -> do liftIO $ putStrLn "IR Tree: "
                          mapM showIRUnit t

          let Right ir' = ir

          breakIf StopTranslate

          oir <- case runOM (optimize ir') of
                   (Left e, _) -> throwError e
                   (Right x,_) -> return x

          liftIO $ putStrLn "Optimized IR: "
          mapM showIRUnit oir

          let cast = cgen oir

          liftIO $ putStrLn "C ast:"
          liftIO $ putStrLn (show cast)

          breakIf StopGen

          let ctext = cprint cprologue cast
          liftIO $ putStrLn "C text:"
          liftIO $ putStrLn ctext

          ifNotOpt NoOutput $ liftIO $ writeFile outC ctext

          return ()
