{-# LANGUAGE TemplateHaskell, BlockArguments 
           , TypeOperators, DataKinds, PolyKinds #-}
module Polysemy.Database
  ( runDbIO
  , selectNoTrans
  , select
  , transact
  )
where

-- Everything for Opaleye
import           Data.Profunctor.Product.Default
                                                ( Default )
import qualified           Opaleye                        as O 
import qualified Database.PostgreSQL.Simple    as PS

-- Within our ecosystem
import qualified Database.Runtime              as RT

-- Polysemy
import           Polysemy
import           Polysemy.Reader

data Db m a where
  -- | Select without transactions: to be used where consistency of reads is not important.
  SelectNoTrans ::Default O.FromFields sql hask => O.Select sql -> Db m [hask]
  -- | Select with transactions.
  Select ::Default O.FromFields sql hask => O.Select sql -> Db m [hask]
  -- | Perform an arbitrary transaction
  Transact ::(PS.Connection -> IO a) -> Db m a

makeSem ''Db

runDbIO
  :: Members '[Embed IO , Reader RT.DBRuntime] r => Sem (Db ': r) a -> Sem r a
runDbIO = interpret $ \case
  SelectNoTrans sel      -> withRuntimeConnection (`O.runSelect` sel)
  Select        sel      -> withRuntimeTransaction (`O.runSelect` sel)
  Transact      withConn -> withRuntimeTransaction withConn

withRuntimeConnection
  :: Members '[Embed IO , Reader RT.DBRuntime] r
  => (PS.Connection -> IO a)
  -> Sem r a
withRuntimeConnection withConn = do
  rt <- ask
  embed . RT.withConnection rt $ withConn

withRuntimeTransaction
  :: Members '[Embed IO , Reader RT.DBRuntime] r
  => (PS.Connection -> IO a)
  -> Sem r a
withRuntimeTransaction withConn =
  withRuntimeConnection $ \conn -> PS.withTransaction conn $ withConn conn
