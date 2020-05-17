module TidyTests.FileUtilsSpec where

import System.Directory (getTemporaryDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import TidyTests.FileUtils

unit_getProjectDir :: IO ()
unit_getProjectDir = do
  tmpdir <- getTemporaryDirectory
  res <- getProjectDir tmpdir
  assertEqual ("should not find project directory above " ++ tmpdir) Nothing res
