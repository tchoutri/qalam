module Qalam.Model.Document.Update
  ( insertDocument
  , deleteDocument
  ) where

import Data.UUID.Types (UUID)
import Effectful
import Qalam.Effect
import Qalam.Effect qualified as Qalam
import Qalam.Model.Document.Types

insertDocument :: Qalam :> es => Document -> Eff es ()
insertDocument document = Qalam.put document.documentId document

deleteDocument :: Qalam :> es => UUID -> Eff es ()
deleteDocument documentId = Qalam.delete documentId
