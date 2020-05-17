{-# LANGUAGE RecordWildCards #-}

module TidyTests
  ( runTidyTests
  ) where

import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode(..), hPutStrLn, stderr, withFile)
import Text.Printf (printf)
import TidyTests.FileUtils

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
          copyInputToTestFile sourceFP
        else do
          errPutStrLn $ printf "Source file %s does not exist." arg
          usage
    _ -> do
      errPutStrLn "wrong number of arguments"
      usage

copyInputToTestFile :: FilePath -> IO ()
copyInputToTestFile sourceFP = do
  let err = error $ printf "Could not find a cabal file above %s." sourceFP
  Paths {..} <- fromMaybe err <$> getPaths sourceFP
  createDirectoryIfMissing True (projectDir </> "test" </> specDirRelPath)
  fileExists <- doesFileExist specFilePath
  if fileExists
    then withFile specFilePath AppendMode $ \h -> do
           hPutStrLn h "" -- a newline just 'cuz
           src <- getContents
           hPutStrLn h src
    else withFile specFilePath WriteMode $ \h -> do
           hPutStrLn h $ printf "module %s where" specModuleName
           src <- getContents
           hPutStrLn h src
