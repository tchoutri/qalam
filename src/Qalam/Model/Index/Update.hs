module Qalam.Model.Index.Update
  ( createIndex
  ) where

import Data.Text (Text)
import Effectful

import Qalam.Effect
import Qalam.Effect qualified as Effect
import Qalam.Model.Index.Types

createIndex :: Qalam :> es => Text -> IndexCondition -> Eff es ()
createIndex field indexCondition = do
  Effect.put
    Index
      { field = field
      , condition = indexCondition
      }
