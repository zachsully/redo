{-
Filename: redo.hs
Part of: redo
Created by: Zach Sullivan
Created on: 06/24/2014
Last Modified by: Zach Sullivan
Last Modified on: 08/28/2014

Redo runs build scripts
-}

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTVar, TVar, readTVar, writeTVar)

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
          
----------- FOR CONCURRENCY UPDATE || NOT WORKING
exeDoFile' options dep = do
  code <- ifM (doesFileExist $ doFile dep) 
          (runReaderT (exeDoFile dep) options)
          (return ExitSuccess)
  return (code, dep)
        
--exeDoFileC :: TVar (IO [(ExitCode, String)]) -> [Opts] -> String -> IO ()  
exeDoFileC var options dep = atomically $ do
  let result = exeDoFile' options dep
  results <- readTVar var
  writeTVar var (result:results)
-----------  

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

type SourceFile = FilePath
type DoFile     = FilePath
type TargetFile = FilePath

data Dependency = Source SourceFile
                | Target DoFile (Maybe TargetFile)
                deriving (Show, Eq)
                         
-- | get's args and options and initially dispatches them
mainN :: IO ()
mainN  = do
  prog <- getProgName
  case prog of
    "redo" -> do (options, args, errs) <- (getOpt Permute optionsList) `liftM` getArgs
                 applyInitOptions (options, errs)
                 mapM_ (\a -> do a' <- parseDependency a
                                 runReaderT (redo a') options) args
    "redo-ifchange" -> redoifchange =<< (mapM parseDependency) =<< getArgs
    "redo-ifcreate" -> redoifcreate =<< getArgs
    
-- | takes a target, executes DoFile, manages temporary files, updates checksums, and exits with appropriate exitcode
redo :: Dependency -> ReaderT [Opts] IO ExitCode
redo (Source _) = liftIO $ exitWith ExitSuccess
redo (Target doFile maybeTargetFile) = do
  options <- ask
  liftIO $ do
    createDirectoryIfMissing True $ sumDirectory target -- is this the right place to be doing this?
    newEnv <- (++ [("REDO_TARGET", target), ("REDO_OPTIONS", show options)]) `liftM` getEnvironment
    (_,_,_,ph) <- createProcess $ (shell $ cmd options) {env = Just newEnv}
    code <- waitForProcess ph
    unless (elem Quiet options) $ printout code
    when (code == ExitSuccess) $ updateTargetSums target
    case maybeTargetFile of
      Nothing         -> return code
      Just targetFile -> if code == ExitSuccess
                         then (renameFile tmpFile targetFile) >> return code 
                         else (removeFile tmpFile) >> return code
  where cmd opts = unwords ["sh", if elem Verbose opts then "-e -v" else "-e"
                           , doFile, "0", target, tmpFile]
        tmpFile = target ++ "---redoing"
        target = takeBaseName doFile
        printout c = case c of
          ExitSuccess    -> hPutStrLn stderr $ target ++ " redone"
          ExitFailure 12 -> hPutStrLn stderr $ target ++ " up to date"
          ExitFailure x  -> hPutStrLn stderr $ target ++ " failed to redo, code: " ++ (show x)

-- | takes a list of deps, calls redo of Targets, and checks the sums exiting with the appropriate exitcode
redoifchange :: [Dependency] -> IO ()
redoifchange deps = do
  options <- read `liftM` getEnv "REDO_OPTIONS"
  out <- forM deps (\d -> do code <- runReaderT (redo d) options
                             return (code, d))
  let errs = filter (\(c,_) -> c /= ExitSuccess || c /= ExitFailure 12) out
  unless (errs == []) $ exitWith $ ExitFailure 99
  when (elem Force options) $ exitWith ExitSuccess
{-
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
-}
-- | takes a list of files, exiting with ExitFailure 12 if it couldn't find files in current directory
redoifcreate :: [FilePath] -> IO ()
redoifcreate files = do
  bools <- mapM doesFileExist files
  if any id bools
    then exitWith ExitSuccess
    else exitWith $ ExitFailure 11

parseDependency :: String -> IO Dependency
parseDependency input = do
  let depDo = doFile input
  hasDo <- doesFileExist depDo
  exists <- doesFileExist input
  case (hasDo, exists) of
    (True, True)   -> return (Target depDo (Just input))
    (True, False)  -> return (Target depDo Nothing)
    (False, True)  -> return (Source input)
    (False, False) -> ioError $ userError $ "'" ++ input ++ "' is not a valid redo candidate"

isUpToDate :: Dependency -> IO Bool    
isUpToDate dep = do
  target <- parseDependency =<< getEnv "REDO_TARGET"
  let oldsumFile = oldChecksumFile target dep
  newsum <- getMD5 $ case dep of (Source path)          -> path
                                 (Target _ (Just path)) -> path
                                 (Target x Nothing)     -> ioError $ userError $ "'" ++ x ++ "' is not a concrete dependency"
  writeFile (newChecksumFile target dep) newsum
  return True

{-  
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
-}    