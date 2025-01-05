module Qalam.Model.Index.Types where

import Codec.Serialise.Class
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Builder.Linear qualified as Linear
import Data.Text.Display
import GHC.Generics

data Index = Index
  { field :: Text
  , condition :: IndexCondition
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)

data IndexCondition
  = Equals Text
  deriving stock (Eq, Ord, Show, Generic)

instance Display IndexCondition where
  displayBuilder (Equals text) = "eq_" <> Linear.fromText text

instance Serialise IndexCondition where
  encode indexCondition = encodeString (display indexCondition)
  decode = do
    textualRepresentation <- decodeString
    convertToCondition textualRepresentation
    where
      convertToCondition t
        | "eq_" `Text.isPrefixOf` t = pure $ Equals (Text.drop 3 t)
        | otherwise = fail $ "Could not recognise index condition " <> Text.unpack t
