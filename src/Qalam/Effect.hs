{-# LANGUAGE IncoherentInstances #-}

module Qalam.Effect
  ( Qalam (..)
  , runQalam
  , put
  , get
  , delete
  ) where

import Codec.Serialise
import Data.ByteString.Lazy qualified as BSL
import Database.RocksDB qualified as RocksDB
import Effectful
import Effectful.Dispatch.Dynamic

data Qalam :: Effect where
  Put :: (Serialise key, Serialise value) => RocksDB.ColumnFamily -> key -> value -> Qalam m ()
  Get :: (Serialise key, Serialise value) => RocksDB.ColumnFamily -> key -> Qalam m (Maybe value)
  Delete :: Serialise key => RocksDB.ColumnFamily -> key -> Qalam m ()

type instance DispatchOf Qalam = Dynamic

runQalam
  :: IOE :> es
  => RocksDB.DB
  -> Eff (Qalam : es) a
  -> Eff es a
runQalam db = interpret_ $ \action -> case action of
  Put cf key value ->
    RocksDB.putCF
      db
      cf
      (BSL.toStrict $ serialise key)
      (BSL.toStrict $ serialise value)
  Get cf key -> do
    mValue <-
      RocksDB.getCF
        db
        cf
        (BSL.toStrict $ serialise key)
    pure $ deserialise . BSL.fromStrict <$> mValue
  Delete cf key -> do
    RocksDB.deleteCF
      db
      cf
      (BSL.toStrict $ serialise key)

put
  :: (Qalam :> es, Serialise key, Serialise value)
  => RocksDB.ColumnFamily
  -> key
  -> value
  -> Eff es ()
put cf key value = send $ Put cf key value

get
  :: (Qalam :> es, Serialise key, Serialise value)
  => RocksDB.ColumnFamily
  -> key
  -> Eff es (Maybe value)
get cf key = send $ Get cf key

delete
  :: (Qalam :> es, Serialise key)
  => RocksDB.ColumnFamily
  -> key
  -> Eff es ()
delete cf key = send $ Delete cf key
