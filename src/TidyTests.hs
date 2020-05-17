module TidyTests
  ( runTidyTests
  ) where

import Control.Monad (unless)
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath
  ( (</>)
  , makeRelative
  , normalise
  , takeDirectory
  , takeFileName
  )
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import TidyTests.FileUtils

errPutStrLn :: String -> IO ()
errPutStrLn = hPutStrLn stderr

getProjectDir' :: FilePath -> IO FilePath
getProjectDir' sourceFP = do
  mRes <- getProjectDir sourceFP
  case mRes of
    Nothing -> do
      errPutStrLn $ printf "Could not find a cabal file above %s." sourceFP
      exitFailure
    Just fp -> return fp

usage :: IO ()
usage = do
  progName <- getProgName
  errPutStrLn $ printf "usage: %s <source filepath>" progName
  exitFailure

runTidyTests :: IO ()
runTidyTests = do
  args <- getArgs
  case args of
    [arg] -> do
      b <- doesFileExist arg
      if b
        then do
          sourceFP <- makeAbsolute arg
          appendInputToMatchingTestFile sourceFP
        else do
          errPutStrLn $ printf "Source file %s does not exist." arg
          usage
    _ -> do
      errPutStrLn "wrong number of arguments"
      usage

appendInputToMatchingTestFile :: FilePath -> IO ()
appendInputToMatchingTestFile sourceFP = do
  projDir <- getProjectDir' sourceFP
  let relPath = makeRelative (projDir </> "src") (takeDirectory sourceFP)
  let testFilename = specName (takeFileName sourceFP)
  let testDirPath = normalise (projDir </> "test" </> relPath)
  putStrLn "Hello, from tidy-tests.  Not yet appending."
  putStrLn $
    printf
      "project-dir=%s; relPath=%s testFilename=%s appendingTo=%s"
      projDir
      relPath
      testFilename
      (testDirPath </> testFilename)
  b <- doesDirectoryExist testDirPath
  unless b $ putStrLn $ printf "Must create directory %s" testDirPath
