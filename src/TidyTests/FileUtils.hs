module TidyTests.FileUtils where

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

getProjectDir :: FilePath -> IO (Maybe FilePath)
getProjectDir sourceFP = go $ takeDirectory sourceFP
  where
    go :: FilePath -> IO (Maybe FilePath)
    go "/" = pure Nothing
    go fp = do
      contents <- listDirectory fp
      if any isCabal contents
        then pure $ Just fp
        else go $ takeDirectory fp
    isCabal fp = takeExtension fp == ".cabal"

getProjectDir' :: FilePath -> IO FilePath
getProjectDir' sourceFP = go $ takeDirectory sourceFP
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
