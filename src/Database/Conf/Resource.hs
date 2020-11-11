{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
: Conf.Resource
Description: Configuration for striped resource pools

A striped resource pool is a pool that is divided into "Stripes". Each stripe has multiple "resources", each of which can be allocated to a particular task.
-}
module Database.Conf.Resource
  ( PoolConf(..)
  , poolConfP
  , pcNumStripes
  , pcKeepOpenDelay
  , pcResPerStripe
  , createPoolFromConf
  , module P
  )
where

import           Control.Lens                   ( makeLenses )
import           Prelude                 hiding ( option )
import           Data.Default.Class             ( Default(..) )
import           Data.Pool                     as P
import qualified Data.Text                     as T
                                                ( unpack )
import           Prelude.Time
import           Options.Applicative

-- | Configuration for a striped resource pool
--
-- See <https://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html#t:Pool>
data PoolConf = PoolConf
  { _pcNumStripes    :: Int             -- ^ Number of stripes to keep open
  , _pcKeepOpenDelay :: NominalDiffTime -- ^ Idle resource timeout
  , _pcResPerStripe  :: Int             -- ^ Number of resources per stripe
  }
  deriving (Eq, Show)

makeLenses ''PoolConf

poolConfP :: Text -> Parser PoolConf
poolConfP pre =
  PoolConf
    <$> option
          auto
          (  long (wpre "num-stripes")
          <> help "Number of stripes in the res. pool"
          <> value _pcNumStripes
          <> metavar "LONG"
          <> showDefault
          )
    <*> option
          readDt
          (  long (wpre "keep-open-time")
          <> help "Amount of time for which an unused resource is kept open"
          <> value _pcKeepOpenDelay
          <> metavar "LONG"
          <> showDefault
          )
    <*> option
          auto
          (  long (wpre "num-res-per-stripe")
          <> help "Number of resources in each stripe"
          <> value _pcResPerStripe
          <> metavar "LONG"
          <> showDefault
          )
 where
  wpre t = T.unpack $ pre <> "-pool-" <> t
  readDt        = eitherReader (fmap secondsToNominalDiffTime . readEither)
  PoolConf {..} = def

instance Default PoolConf where
  def = PoolConf 1 (secondsToNominalDiffTime 5) 10

-- | Create a `P.Pool` from a given `PoolConf` and the additional resource handling functions
--
-- See: <https://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html#t:Pool>
createPoolFromConf
  :: PoolConf      -- ^ Pool configuration to use
  -> IO a          -- ^ An action that creates a new resource
  -> (a -> IO ())  -- ^ An action that destroys/closes a resource
  -> IO (P.Pool a) -- ^ The pool
createPoolFromConf PoolConf {..} create close =
  P.createPool create close _pcNumStripes _pcKeepOpenDelay _pcResPerStripe
