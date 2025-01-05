module Main where

import Database.RocksDB
import System.IO
import Test.Tasty
import Test.Tasty.Runners.Reporter qualified as Reporter
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem (FileSystem)
import Effectful

import Test.TestingUtils
import Test.Document qualified as DocumentTest

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  runEff . FileSystem.runFileSystem $
    cleanUp
  let config =
        Config
          { createIfMissing = True
          , errorIfExists = False
          , paranoidChecks = False
          , maxFiles = Nothing
          , prefixLength = Nothing
          , bloomFilter = True
          }
  let testEnv = TestEnv "test/test.db" config
  spec <- traverse (\comp -> runTestEff comp testEnv) specs
  defaultMainWithIngredients [Reporter.ingredient] $
    testGroup "Qalam Tests" spec

specs :: [TestEff TestTree]
specs = [
  DocumentTest.spec
  ]

cleanUp :: FileSystem :> es => Eff es ()
cleanUp = do
  FileSystem.removeDirectoryRecursive "test/test.db"
