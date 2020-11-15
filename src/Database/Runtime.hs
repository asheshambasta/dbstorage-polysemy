{-# LANGUAGE StrictData #-}
module Database.Runtime
  ( DBRuntime(_dbrConf)
  -- ** Create and destroy
  , createRuntime
  , destroyRuntime
  -- ** Consuming connections
  , withConnection
  , module Conf
  )
where

import           Database.Conf                 as Conf
import qualified Database.PostgreSQL.Simple    as PS

-- | A Database runtime: contains all resources for DB operations to be executed. 
data DBRuntime = DBRuntime
  { _dbrConf     :: Conf.DBConf -- ^ The configuration of this runtime
  , _dbrConnPool :: Conf.DBConnPool -- ^ Current connection pool
  }

-- | Create a DB runtime given some `Conf.DBConf` 
createRuntime :: Conf.DBConf -> IO DBRuntime
createRuntime conf@Conf.DBConf {..} =
  DBRuntime conf <$> Conf.createPoolDB _dbConnStr _dbPoolConf

-- | Close all connections 
-- FIXME: check if closing connections via `destroyAllResources` is safe. 
-- We call `PS.close`, however, does that close ongoing transactions safely?
destroyRuntime :: DBRuntime -> IO ()
destroyRuntime = Conf.destroyAllResources . _dbrConnPool

-- | Given a function that consumes a `PS.Connection` to provide some result @a@,
-- execute that function with a connection from the pool.
withConnection :: DBRuntime -> (PS.Connection -> IO a) -> IO a
withConnection DBRuntime {..} = withResource _dbrConnPool
