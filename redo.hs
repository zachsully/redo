{-
Filename: redo.hs
Part of: redo
Created by: Zach Sullivan
Created on: 06/24/2014
Last Modified by: Zach Sullivan
Last Modified on: 09/02/2014

Redo runs build scripts
-}

import Control.Conditional (ifM, whenM)
import Control.Exception.Base (evaluate)
import Control.Monad (forM, liftM, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import qualified Data.ByteString.Lazy as BSL (readFile)
import Data.Digest.Pure.MD5 (md5)
import System.Console.GetOpt (OptDescr (..), ArgDescr (NoArg), ArgOrder (Permute), getOpt)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getDirectoryContents, removeFile, renameFile)
import System.Environment (getArgs, getEnv, getEnvironment, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath.Posix (takeBaseName, takeExtension)
import System.IO (hClose, hFlush, hGetContents, hPutStr, hPutStrLn, IOMode (..), openFile, stderr, withFile)
import System.IO.Error (ioError, userError)
import System.Process (CreateProcess (..), createProcess, shell, waitForProcess)

data Opts = Concurrent -- NOT IMPLEMENTED
          | Force
          | Help
          | Quiet
          | Verbose
          | Version -- NOT IMPLEMENTED
          deriving (Eq, Read, Show)

---------------------------------------------------------------------------------------------------------

main :: IO ()
main  = do
  prog <- getProgName
  case prog of 
    "redo" -> do (options, targets, errs) <- (getOpt Permute optionsList) `liftM` getArgs
                 applyInitOptions (options, errs)
                 mapM_ (\t -> runReaderT (exeDoFile t) options) targets
                 
    -- THESE ARE MEANT TO RUN INSIDE A TARGETS DOFILE                 
    "redo-ifchange" -> redoIfChange =<< getArgs
    "redo-ifcreate" -> redoIfCreate =<< getArgs
    
------------------------------------------------------------------------------------------

-- | takes a list of dependances, exits ExitFailure 12 if deps are up to date
redoIfChange :: [String] -> IO ()
redoIfChange deps = do
  options <- read `liftM` getEnv "REDO_OPTIONS"
  out <- processOutput `liftM` mapM (exeDoFile' options) deps
  when (elem Force options) $ exitWith ExitSuccess -- if Force option ExitSuccess, continue do file
  if (fst out) == ExitSuccess || (fst out) == ExitFailure 12
    then do changes <- mapM hasChanged deps
            when (elem Verbose options) $ hPutStrLn stderr $ show changes
            if all (== False) changes
              then exitWith $ ExitFailure 12
              else exitWith ExitSuccess
    else exitWith $ fst out

redoIfCreate :: [String] -> IO ()
redoIfCreate deps = do
  created <- mapM doesFileExist deps
  if all (== False) created
    then exitWith $ ExitFailure 111
    else return ()
         
----------------------------------------------------------------------------------------
  
-- | takes parsed options and applies the ones that are meant to run initially
applyInitOptions :: ([Opts], [String]) -> IO ()               
applyInitOptions (options, errs) = do
  when (errs /= []) (do hPutStr stderr $ "redo:   "
                                         ++ (tail $ foldr (\e rest -> "\t" ++ e ++ rest) "" errs)
                                         ++ "\nfor help, try --help\n"
                        exitWith $ ExitFailure 2)
  when (elem Help options) (do hPutStrLn stderr =<< readFile "README.md"
                               exitWith ExitSuccess)
  when (elem Verbose options) $ hPutStrLn stderr showOpts
  where showOpts = "redo invoked with" ++ (tail $ foldr (\o rest -> ", " ++ o ++ rest) " flag(s).\n" $ map show options)

oldChecksumFile :: String -> String -> FilePath
oldChecksumFile  = \target dep -> concat [".redo/", target, "/", dep, ".oldsum"]

newChecksumFile :: String -> String -> FilePath
newChecksumFile  = \target dep -> concat [".redo/", target, "/", dep, ".newsum"]

-- | takes a target and returns the filepath to it's doFile (build script)
doFile :: String -> FilePath
doFile  = \target -> target ++ ".do"

-- | takes a target and runs it's doFile with certain cmd Options, returning the exitCode of the target
exeDoFile :: String -> ReaderT [Opts] IO ExitCode
exeDoFile target = do
  options <- ask
  liftIO $ do
    createDirectoryIfMissing True $ sumDirectory target
    newEnv <- (++ [("REDO_TARGET", target), ("REDO_OPTIONS", show options)]) `liftM` getEnvironment
    (_,_,_,ph) <- createProcess $ (shell $ cmd options) {env = Just newEnv}
    code <- waitForProcess ph
    manageTmpFile code
    unless (elem Quiet options) $ printout code
    when (code == ExitSuccess) $ updateTargetSums target
    return code
  where cmd opts = unwords ["sh", if elem Verbose opts then "-e -v" else "-e"
                           , doFile target, "0", target, tmpFile]
        tmpFile = target ++ "---redoing"
        manageTmpFile c = whenM (doesFileExist tmpFile)
                                (if c == ExitSuccess
                                 then renameFile tmpFile target
                                 else removeFile tmpFile) 
        printout c = case c of
          ExitSuccess    -> hPutStrLn stderr $ target ++ " redone"
          ExitFailure 12 -> hPutStrLn stderr $ target ++ " up to date"
          ExitFailure x  -> hPutStrLn stderr $ target ++ " failed to redo, code: " ++ (show x)
          
exeDoFile' options dep = do
  code <- ifM (doesFileExist $ doFile dep) 
          (runReaderT (exeDoFile dep) options)
          (return ExitSuccess)
  return (code, dep)
        
-- | takes a file and returns it's md5 as a string
getMD5 :: FilePath -> IO String
getMD5  = \path -> (show . md5) `liftM` BSL.readFile path

-- | takes a target and a dependency and returns True if the target has changed or is missing
hasChanged :: String -> IO Bool
hasChanged dep = do
  -- let
  target <- getEnv "REDO_TARGET"
  newsum <- getMD5 dep
  let oldsumFile = oldChecksumFile target dep
  -- in
  writeFile (newChecksumFile target dep) newsum
  trueUnlessM (doesFileExist oldsumFile)
    $ trueUnlessM (doesFileExist dep)
      $ withFile oldsumFile ReadMode (\h -> evaluate =<< (newsum /=) `liftM` hGetContents h)
  where trueUnlessM :: Monad m => m Bool -> m Bool -> m Bool
        trueUnlessM bool expr = bool >>= (\inp -> if inp then expr else return True)
        
optionsList :: [OptDescr Opts]
optionsList  = [ Option ['c'] ["concurrent"] (NoArg Concurrent) "redo targets and check dependencies concurrently"
               , Option ['f'] ["force"] (NoArg Force) "Force redo, even if up to date"
               , Option ['h'] ["help"] (NoArg Help) "Show help dialog"
               , Option ['q'] ["quiet"] (NoArg Quiet) "Hides output"
               , Option ['v'] ["verbose"] (NoArg Verbose) "Print extra information"
               , Option ['V'] ["version"] (NoArg Version) "Show version information from cabal file"]
               
-- | takes a list of code, target pairs and returns the first pair with an error, else the first pair
processOutput :: [(ExitCode, String)] -> (ExitCode, String)               
processOutput (last: []) = last
processOutput (current:rest) = if (fst current) == ExitSuccess
                               then processOutput rest
                               else current
                                    
sumDirectory :: String -> FilePath
sumDirectory  = \target -> concat [".redo/", target, "/"]

-- | replaces oldsums with newsum in dependency database
updateTargetSums :: String -> IO ()
updateTargetSums target = do
  let directory = sumDirectory target
  contents <- getDirectoryContents directory
  mapM_ (\file -> case takeExtension file of
            ".newsum" -> renameFile (directory ++ file) (directory ++ (takeBaseName file) ++ ".oldsum")
            _         -> return ()) contents

{- 
To Do:

1. Checking and updating of dependencies concurrently
2. default.*.do directory
3. Option for redo-ifchange
    that says if a target is abstract, meaning that it does not build a particular file
ALWAYS - Further code optimization

4. Cache working source files to allow people to revert to last running state
-}