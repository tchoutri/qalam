module Qalam.Effect where

import Codec.Serialise
import Data.ByteString.Lazy qualified as BSL
import Database.RocksDB qualified as RocksDB
import Effectful
import Effectful.Dispatch.Dynamic
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

data Qalam :: Effect where
  Put :: (Serialise key, Serialise value) => key -> value -> Qalam m ()
  Get :: (Serialise key, Serialise value) => key -> Qalam m (Maybe value)
  Delete :: Serialise key => key -> Qalam m ()

type instance DispatchOf Qalam = Dynamic

runQalam
  :: IOE :> es
  => OsPath
  -- ^ Database
  -> RocksDB.Config
  -- ^ Configuration
  -> Eff (Qalam : es) a
  -> Eff es a
runQalam path config = interpret_ $ \action -> do
  filepath <- liftIO $ OsPath.decodeFS path
  RocksDB.withDBCF filepath config [("datastore", config), ("indexes", config)] $
    \db -> case action of
      Put key value ->
        RocksDB.putCF
          db
          (head db.columnFamilies)
          (BSL.toStrict $ serialise key)
          (BSL.toStrict $ serialise value)
      Get key -> do
        mValue <-
          RocksDB.getCF
            db
            (head db.columnFamilies)
            (BSL.toStrict $ serialise key)
        pure $ deserialise . BSL.fromStrict <$> mValue
      Delete key -> do
        RocksDB.deleteCF
          db
          (head db.columnFamilies)
          (BSL.toStrict $ serialise key)

put :: (Qalam :> es, Serialise key, Serialise value) => key -> value -> Eff es ()
put key value = send $ Put key value

get :: (Qalam :> es, Serialise key, Serialise value) => key -> Eff es (Maybe value)
get key = send $ Get key

delete :: (Qalam :> es, Serialise key) => key -> Eff es ()
delete key = send $ Delete key
