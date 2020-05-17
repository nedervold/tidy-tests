module TidyTests
  ( runTidyTests
  ) where

import Control.Monad (unless)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  , makeAbsolute
  )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath
  ( (<.>)
  , (</>)
  , makeRelative
  , normalise
  , splitExtension
  , takeDirectory
  , takeExtension
  , takeFileName
  )
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

errPutStrLn :: String -> IO ()
errPutStrLn = hPutStrLn stderr

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
  projDir <- projectDir sourceFP
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

specName :: FilePath -> FilePath
specName fn = basename ++ "Spec" <.> ext
  where
    (basename, ext) = splitExtension fn

projectDir :: FilePath -> IO FilePath
projectDir sourceFP = go $ takeDirectory sourceFP
  where
    go :: FilePath -> IO FilePath
    go "/" = do
      errPutStrLn $ printf "Could not find a cabal file above %s." sourceFP
      exitFailure
    go fp = do
      contents <- listDirectory fp
      if any isCabal contents
        then return fp
        else go $ takeDirectory fp
    isCabal fp = takeExtension fp == ".cabal"
