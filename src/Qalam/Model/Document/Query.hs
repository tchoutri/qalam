module Qalam.Model.Document.Query
  ( getDocumentById
  ) where

import Data.UUID.Types (UUID)
import Effectful

import Qalam.Effect (Qalam)
import Qalam.Effect qualified as Qalam
import Qalam.Model.Document.Types (Document)

getDocumentById :: Qalam :> es => UUID -> Eff es (Maybe Document)
getDocumentById documetId = Qalam.get documentId
