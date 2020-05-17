module TidyTests.FileUtilsSpec where

import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath
import System.IO.Temp(withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import TidyTests.FileUtils

unit_getProjectDir_noProject :: IO ()
unit_getProjectDir_noProject = do
  tmpdir <- getTemporaryDirectory
  res <- getProjectDir tmpdir
  assertEqual ("should not find project directory above " ++ tmpdir) Nothing res

unit_getProjectDir_withProject :: IO ()
unit_getProjectDir_withProject = do
  withSystemTempDirectory "tidy-tests" $ \ dir -> do
      touch (dir </> "foo.cabal")
      createDirectoryIfMissing True (dir </> "src/a/b/c/d/e/f")
      touch  (dir </> "src/a/b/c/d/e/f/G.hs")
      let filepath =  normalise (dir </> "src/a/b/c/d/e/f/G.hs")
      res <- getProjectDir filepath
      assertEqual "should find project directory" (Just $ normalise dir) res

touch :: FilePath -> IO ()
touch fp = writeFile fp ""