module Qalam.Model.Index.Query where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Effectful

import Qalam.Effect (Qalam)
import Qalam.Effect qualified as Effect

getIndexes :: Qalam :> es => Map Text (Text -> Bool)
getIndexes = send $ Get
