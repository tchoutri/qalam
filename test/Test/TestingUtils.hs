module Test.TestingUtils
  ( TestEff
  , TestEnv (..)
  , runTestEff
  , testThis
  , testThese

    -- ** Assertion functions
  , assertBool
  , assertEqual
  , assertFailure
  , assertJust

    -- ** Documents
  , DocumentTemplate (..)
  , randomDocumentTemplate
  , instantiateRandomDocument
  ) where

import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.UUID.Types (UUID)
import Database.RocksDB qualified as RocksDB
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import GHC.Generics
import GHC.Stack
import Heptapod qualified as UUID
import Test.QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty qualified as Test
import Test.Tasty.HUnit qualified as Test

import Qalam.Effect
import Qalam.Model.Document.Types
import Qalam.Model.Document.Update qualified as Update
import Test.Orphans ()

data TestEnv = TestEnv
  { database :: RocksDB.DB
  }

type TestEff =
  Eff
    '[ Qalam
     , Reader TestEnv
     , IOE
     ]

runTestEff :: TestEff a -> TestEnv -> IO a
runTestEff comp env@TestEnv{databasePath, databaseConfig} =
  comp
    & runQalam databasePath databaseConfig
    & Reader.runReader env
    & runEff

testThis :: String -> TestEff () -> TestEff TestTree
testThis name assertion = do
  env <- Reader.ask @TestEnv
  let test = runTestEff assertion env
  pure $ Test.testCase name test

testThese :: String -> [TestEff TestTree] -> TestEff TestTree
testThese groupName tests = fmap (Test.testGroup groupName) newTests
  where
    newTests :: TestEff [TestTree]
    newTests = sequenceA tests

assertBool :: Bool -> TestEff ()
assertBool boolean = liftIO $ Test.assertBool "" boolean

-- | Make sure an expected value is the same as the actual one.
--
--  Usage:
--
--  >>> assertEqual expected actual
assertEqual :: (Eq a, Show a) => a -> a -> TestEff ()
assertEqual expected actual = liftIO $ Test.assertEqual "" expected actual

assertFailure :: MonadIO m => String -> m ()
assertFailure = liftIO . Test.assertFailure

assertJust :: HasCallStack => Maybe a -> TestEff a
assertJust (Just a) = pure a
assertJust Nothing = liftIO $ Test.assertFailure "Test return Nothing instead of Just"

data DocumentTemplate m = DocumentTemplate
  { version :: m Word
  , documentId :: m UUID
  , documentData :: m (Map DocumentData DocumentData)
  }
  deriving stock (Generic)

randomDocumentTemplate :: MonadIO m => DocumentTemplate m
randomDocumentTemplate =
  DocumentTemplate
    { version = pure 0
    , documentId = liftIO UUID.generate
    , documentData = liftIO $ generate arbitrary
    }

instantiateRandomDocument
  :: Qalam :> es
  => DocumentTemplate (Eff es)
  -> Eff es Document
instantiateRandomDocument
  DocumentTemplate
    { version = generateVersion
    , documentId = generateDocumentId
    , documentData = generateDocumentData
    } = do
    version <- generateVersion
    documentId <- generateDocumentId
    documentData <- generateDocumentData
    let document = Document{..}
    Update.insertDocument document
    pure document
