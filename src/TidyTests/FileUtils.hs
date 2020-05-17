module TidyTests.FileUtils
  ( Paths(..)
  , getPaths
  , getProjectDir
  , mkPaths
  , slashesToDots
  , specName
  ) where

import System.Directory (listDirectory, makeAbsolute)
import System.FilePath
  ( (<.>)
  , (</>)
  , dropExtension
  , makeRelative
  , splitExtension
  , takeDirectory
  , takeExtension
  , takeFileName
  )

getProjectDir :: FilePath -> IO (Maybe FilePath)
getProjectDir sourceFP = do
  sourceFP' <- makeAbsolute sourceFP
  go $ takeDirectory sourceFP'
  where
    go :: FilePath -> IO (Maybe FilePath)
    go "/" = pure Nothing
    go fp = do
      contents <- listDirectory fp
      if any isCabal contents
        then pure $ Just fp
        else go $ takeDirectory fp
    isCabal fp = takeExtension fp == ".cabal"

data Paths = Paths
  { projectDir :: FilePath
  , srcFilePath :: FilePath
  , srcFileRelPath :: FilePath
  , srcDirRelPath :: FilePath
  , srcFileName :: FilePath
  , srcModuleName :: String
  , specFilePath :: FilePath
  , specFileRelPath :: FilePath
  , specDirRelPath :: FilePath
  , specFileName :: FilePath
  , specModuleName :: String
  } deriving (Show)

mkPaths :: FilePath -> FilePath -> Paths
mkPaths projDir srcFP =
  Paths
    { projectDir = projDir
    , srcFilePath = srcFP
    , srcFileRelPath = sfrp
    , srcDirRelPath = sdrp
    , srcFileName = basename
    , srcModuleName = modName sfrp
    , specFilePath = projDir </> "test" </> sdrp </> spec
    , specFileRelPath = sdrp </> spec
    , specDirRelPath = sdrp
    , specFileName = spec
    , specModuleName = modName (sdrp </> spec)
    }
  where
    sfrp = makeRelative (projDir </> "src") srcFP
    sdrp = takeDirectory sfrp
    basename = takeFileName srcFP
    spec = specName basename

getPaths :: FilePath -> IO (Maybe Paths)
getPaths srcFP = do
  mProjDir <- getProjectDir srcFP
  pure $ fmap mkPaths' mProjDir
  where
    mkPaths' :: FilePath -> Paths
    mkPaths' projDir = mkPaths projDir srcFP

specName :: FilePath -> FilePath
specName fn = basename ++ "Spec" <.> ext
  where
    (basename, ext) = splitExtension fn

modName :: FilePath -> String
modName fp = slashesToDots $ dropExtension fp

slashesToDots :: String -> String
slashesToDots = map slashToDot
  where
    slashToDot '/' = '.'
    slashToDot c = c
