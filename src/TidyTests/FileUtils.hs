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

getDirPathRelativeToSrcDir :: FilePath -> FilePath -> FilePath
getDirPathRelativeToSrcDir projDir srcFilepath =
  makeRelative (projDir </> "src") (takeDirectory srcFilepath)

getDirPathRelativeToTestDir :: FilePath -> FilePath -> FilePath
getDirPathRelativeToTestDir projDir srcFilepath =
  projDir </> "test" </> getDirPathRelativeToSrcDir projDir srcFilepath
