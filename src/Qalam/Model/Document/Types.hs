{-# OPTIONS_GHC -Wno-orphans #-}

module Qalam.Model.Document.Types where

import Codec.Serialise.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import Effectful
import GHC.Generics
import Heptapod

newtype DocumentData = DocumentData Text
  deriving newtype (Eq, Ord, Show, Serialise)

data Document = Document
  { version :: Word
  , documentId :: UUID
  , documentData :: Map DocumentData DocumentData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)

instance Serialise UUID where
  encode = encode . UUID.toText
  decode = do
    decodedText <- decode @Text
    case UUID.fromText decodedText of
      Just x -> pure x
      Nothing -> fail "Could not decode UUID"

newDocument :: IOE :> es => Map DocumentData DocumentData -> Eff es Document
newDocument documentData = do
  documentId <- generate
  let version = 0
  pure Document{..}
