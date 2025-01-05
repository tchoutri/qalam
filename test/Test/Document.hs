module Test.Document where

import Test.Tasty

import Qalam.Model.Document.Query qualified as Query
import Qalam.Model.Document.Types
import Test.TestingUtils

spec :: TestEff TestTree
spec =
  testThese
    "Document Operations"
    [ testThis "Insertion and Retrieval" testDocumentInsertionAndRetrieval
    ]

testDocumentInsertionAndRetrieval :: TestEff ()
testDocumentInsertionAndRetrieval = do
  document1 <- instantiateRandomDocument randomDocumentTemplate
  actualDocument <- assertJust =<< Query.getDocumentById document1.documentId
  assertEqual
    actualDocument
    document1
