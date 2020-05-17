module TidyTests.FileUtilsSpec where

import Hedgehog
import Hedgehog.Gen (alphaNum, list, upper)
import Hedgehog.Range (linear)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
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

filePathSegment :: MonadGen m => m String
filePathSegment = (:) <$> upper <*> list rng alphaNum
  where
    rng = linear 0 15

sourceFileName :: MonadGen m => m String
sourceFileName = (++ ".hs") <$> filePathSegment

sourceFilePath :: MonadGen m => FilePath -> m FilePath
sourceFilePath projectDir = do
  fp <- filePath
  srcFN <- sourceFileName
  return (projectDir </> "src" </> fp </> srcFN)

filePath :: MonadGen m => m FilePath
filePath = foldl1 (</>) <$> list rng filePathSegment
  where
    rng = linear 1 16

hprop_getDirPathRelativeToSrcDir :: Property
hprop_getDirPathRelativeToSrcDir =
  property $ do
    (projectDir, srcPath) <-
      forAll $ do
        projectDir <- filePath
        srcPath <- sourceFilePath projectDir
        return (projectDir, srcPath)
    let path = getDirPathRelativeToSrcDir projectDir srcPath
    projectDir </> "src" </> path === takeDirectory srcPath
