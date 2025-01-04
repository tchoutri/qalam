module Test.Document where

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Document Operations"
    [ testCase "Insertion and Retrieval" testDocumentInsertionAndRetrieval
    ]

testDocumentInsertionAndRetrieval :: Assertion
testDocumentInsertionAndRetrieval = undefined
