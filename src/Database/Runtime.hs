{-# LANGUAGE StrictData #-}
module Database.Runtime
  ( DBRuntime(..)
  , createRuntime
  )
where

import qualified Database.Conf                 as Conf
import           Data.Pool
import qualified Database.PostgreSQL.Simple    as PS
import           Polysemy

-- | A Database runtime: contains all resources for DB operations to be executed. 
data DBRuntime = DBRuntime
  { _dbrConf     :: Conf.DBConf -- ^ The configuration of this runtime
  , _dbrConnPool :: Pool PS.Connection -- ^ Current connection pool
  }

-- | Create a DB runtime given some `Conf.DBConf` 
createRuntime :: Conf.DBConf -> IO DBRuntime
createRuntime conf@Conf.DBConf {..} =
  DBRuntime conf <$> Conf.createPoolDB _dbConnStr _dbPoolConf

