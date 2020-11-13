{-# LANGUAGE TemplateHaskell, BlockArguments 
           , TypeOperators, DataKinds, PolyKinds
           , ConstraintKinds #-}
module Polysemy.Database
  ( runDbIO
  , selectNoTrans
  , select
  , transact
  , transactConn
  , Transaction(..)

  -- ** Transactional 
  , trSelect
  , trUpdate
  , trDelete
  , trInsert
  -- * Constriant
  , Runtime
  , Transact
  -- * Re-exports
  , O.Select
  , O.Update
  , O.Insert
  , O.Delete
  , module RT
  , module Data.Profunctor.Product.Default
  , module Polysemy
  , module Polysemy.Reader
  )
where

-- Everything for Opaleye
import           Data.Profunctor.Product.Default
                                                ( Default )
import qualified Opaleye                       as O
import qualified Database.PostgreSQL.Simple    as PS

-- Within our ecosystem
import           Database.Runtime              as RT

-- Polysemy
import           Polysemy
import           Polysemy.Reader

data Transaction m a where
  TrSelect ::Default O.FromFields sql hask => O.Select sql -> Transaction m [hask]
  TrUpdate ::O.Update hask -> Transaction m [hask]
  TrDelete ::O.Delete hask -> Transaction m [hask]
  TrInsert ::O.Insert hask -> Transaction m [hask]
  -- todo add others.

makeSem ''Transaction

-- runTransactionIO :: Members '[Embed IO, Reader PS.Connection] r => Sem (Transaction ': r) a -> Sem r a 
-- runTransactionIO = interpret $ undefined 
runTransactionIO
  :: Sem '[Transaction , Reader PS.Connection , Embed IO] a
  -> Sem '[Reader PS.Connection , Embed IO] a
runTransactionIO = interpret $ undefined

type Transact r = Members '[Embed IO , Reader PS.Connection] r

data Db m a where
  -- | Select without transactions: to be used where consistency of reads is not important.
  SelectNoTrans ::Default O.FromFields sql hask => O.Select sql -> Db m [hask]
  -- | Select with transactions.
  Select ::Default O.FromFields sql hask => O.Select sql -> Db m [hask]
  -- | Perform an arbitrary transaction
  TransactConn ::(PS.Connection -> IO a) -> Db m a
  Transact ::Sem '[Transaction, Reader PS.Connection, Embed IO] a -> Db m a

makeSem ''Db

runDbIO
  :: Members '[Embed IO , Reader RT.DBRuntime] r => Sem (Db ': r) a -> Sem r a
runDbIO = interpret $ \case
  SelectNoTrans sel      -> withRuntimeConnection (`O.runSelect` sel)
  Select        sel      -> withRuntimeTransaction (`O.runSelect` sel)
  TransactConn  withConn -> withRuntimeTransaction withConn
  Transact      trans'   -> withRuntimeTransaction
    $ \conn -> runM $ Polysemy.Reader.runReader conn (runTransactionIO trans')

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

type Runtime r = Members '[Embed IO , Reader RT.DBRuntime] r
