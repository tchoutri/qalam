module Test.TestingUtils where

import Data.Map.Strict (Map)
import Data.UUID.Types (UUID)
import Effectful
import GHC.Generics
import Heptapod qualified as UUID
import Test.QuickCheck

import Qalam.Effect
import Qalam.Model.Document.Types
import Qalam.Model.Document.Update qualified as Update
import Test.Orphans ()

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
    , documentId = liftIO $ UUID.generate
    , documentData = liftIO $ generate arbitrary
    }

instantiateRandomDocument
  :: (Qalam :> es)
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

