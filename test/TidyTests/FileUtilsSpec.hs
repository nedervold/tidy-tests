{-# LANGUAGE RecordWildCards #-}

module TidyTests.FileUtilsSpec where

import Hedgehog
import Hedgehog.Gen (alphaNum, list, upper)
import Hedgehog.Range (linear)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.HUnit
import TidyTests.FileUtils

unit_getProjectDir_noProject :: IO ()
unit_getProjectDir_noProject = do
  tmpdir <- getTemporaryDirectory
  res <- getProjectDir tmpdir
  assertEqual ("should not find project directory above " ++ tmpdir) Nothing res

unit_getProjectDir_withProject :: IO ()
unit_getProjectDir_withProject =
  withSystemTempDirectory "tidy-tests" $ \dir -> do
    touch (dir </> "foo.cabal")
    createDirectoryIfMissing True (dir </> "src/a/b/c/d/e/f")
    touch (dir </> "src/a/b/c/d/e/f/G.hs")
    let filepath = normalise (dir </> "src/a/b/c/d/e/f/G.hs")
    res <- getProjectDir filepath
    assertEqual "should find project directory" (Just $ normalise dir) res
  where
    touch :: FilePath -> IO ()
    touch fp = writeFile fp ""

projectDirAndSrcPath :: MonadGen m => m (FilePath, FilePath)
projectDirAndSrcPath = do
  projectDir' <- filePath
  srcPath' <- sourceFilePath' projectDir'
  return (projectDir', srcPath')

filePathSegment :: MonadGen m => m String
filePathSegment = (:) <$> upper <*> list rng alphaNum
  where
    rng = linear 0 5

sourceFileName :: MonadGen m => m String
sourceFileName = (++ ".hs") <$> filePathSegment

sourceFilePath' :: MonadGen m => FilePath -> m FilePath
sourceFilePath' projectDir = do
  fp <- filePath
  srcFN <- sourceFileName
  return (projectDir </> "src" </> fp </> srcFN)

filePath :: MonadGen m => m FilePath
filePath = foldl1 (</>) <$> list rng filePathSegment
  where
    rng = linear 1 6

paths :: MonadGen m => m Paths
paths = do
  (pd, sp) <- projectDirAndSrcPath
  pure $ mkPaths pd sp

hprop_mkPaths :: Property
hprop_mkPaths =
  property $ do
    (projectDir', srcPath) <- forAll projectDirAndSrcPath
    let Paths {..} = mkPaths projectDir' srcPath
    srcFilePath === projectDir </> "src" </> srcFileRelPath
    srcFileRelPath === srcDirRelPath </> srcFileName
    projectDir </> "src" </> srcDirRelPath === takeDirectory srcFilePath
    projectDir </> "src" </> srcDirRelPath </> srcFileName === srcFilePath
    srcModuleName === reverse (drop 3 $ reverse $ slashesToDots srcFileRelPath)
    specDirRelPath === srcDirRelPath
    specModuleName === srcModuleName ++ "Spec"
    specFilePath === projectDir </> "test" </> specFileRelPath
    specFileRelPath === specDirRelPath </> specFileName
    projectDir </> "test" </> specDirRelPath === takeDirectory specFilePath
    projectDir </> "test" </> specDirRelPath </> specFileName === specFilePath
    specModuleName ===
      reverse (drop 3 $ reverse $ slashesToDots specFileRelPath)
