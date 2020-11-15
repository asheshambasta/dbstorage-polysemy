{-# LANGUAGE TemplateHaskell, BlockArguments 
           , TypeOperators, DataKinds, PolyKinds
           , ConstraintKinds #-}
module Polysemy.Database
  ( runDbIO
  , selectNoTrans
  , select
  -- , transact
  -- , transactConn
  , Transaction(..)

  -- ** Transactional 
  , trSelect
  , trUpdateReturning
  , trInsertManyReturning
  , trDelete
  , trDeleteReporting
  , trInsert

  , runTransactionEmbed
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
import           Polysemy.Input
import           Polysemy.Reader

data Transaction m a where
  TrSelect ::Default O.FromFields sql hask => O.Select sql -> Transaction m [hask]

  -- | Update a row returning some function of columns from the affected rows.
  TrUpdateReturning ::Default O.QueryRunner colsReturned hask
    => O.Table colsW colsR -- ^ Target table
    -> (colsR -> colsW)  -- ^ Update function
    -> (colsR -> O.Column O.SqlBool) -- ^ WHERE 
    -> (colsR -> colsReturned) -- ^ Extract return values from affected rows.
    -> Transaction m [hask]

  TrInsertManyReturning ::Default O.QueryRunner colsReturned hask
    => O.Table colsW colsR -- ^ Table to insert into 
    -> [colsW] -- ^ Rows to insert 
    -> (colsR -> colsReturned) -- ^ Get affected cols, one wishes to report.
    -> Transaction m [hask]

  -- TrDeleteReturning
  --   ::Default O.QueryRunner columnsReturned hask
  --   => O.Table a columnsR
  --   -> (columnsR -> O.Column O.SqlBool)
  --   -> (columnsR -> columnsReturned)
  --   -> Transaction m [hask]

  TrDelete ::O.Table a colsR -> (colsR -> O.Column O.SqlBool) -> Transaction m Int64

  TrDeleteReporting ::id -> O.Table a colsR -> (colsR -> O.Column O.SqlBool) -> Transaction m [id]

  TrInsert ::O.Insert hask -> Transaction m [hask]

  -- todo add others.

makeSem ''Transaction

-- | Run a bulk of operations within a transaction.
runTransactionWithConn
  :: Member (Embed IO) r
  => Sem (Transaction ': r) a
  -> Sem (Input PS.Connection ': r) a
runTransactionWithConn = reinterpret $ \case

  TrSelect sel -> withInputConn (`O.runSelect` sel)

  TrInsertManyReturning table rows returning' ->
    withInputConn $ \conn -> O.runInsertManyReturning conn table rows returning'

  TrDelete table where' ->
    withInputConn $ \conn -> O.runDelete conn table where'
  TrDeleteReporting id table where' -> withInputConn $ \conn ->
    O.runDelete conn table where' <&> flip replicate id . fromIntegral

  TrUpdateReturning table updateRows where' returning' -> withInputConn
    $ \conn -> O.runUpdateReturning conn table updateRows where' returning'

  -- TrUpdate u -> withInputConn (`O.runUpdate` u)
  -- do
    -- conn <- input
    -- embed $ withTransaction (`O.runSelect` sel) conn
  -- where withTransaction withConn conn = PS.withTransaction conn $ withConn conn

withInputConn
  :: Member (Embed IO) r
  => (PS.Connection -> IO a)
  -> Sem (Input PS.Connection : r) a
withInputConn withConn = input >>= embed . withConn

-- -- runTransactionIO :: Members '[Embed IO, Reader PS.Connection] r => Sem (Transaction ': r) a -> Sem r a 
-- -- runTransactionIO = interpret $ undefined 
-- runTransactionIO
--   :: Sem '[Transaction , Reader PS.Connection , Embed IO] a
--   -> Sem '[Reader PS.Connection , Embed IO] a
-- runTransactionIO = interpret $ undefined

type Transact r = Members '[Embed IO , Reader PS.Connection] r

data Db m a where
  -- | Select without transactions: to be used where consistency of reads is not important.
  SelectNoTrans ::Default O.FromFields sql hask => O.Select sql -> Db m [hask]
  -- | Select with transactions.
  Select ::Default O.FromFields sql hask => O.Select sql -> Db m [hask]
  -- | Perform an arbitrary transaction
  -- TransactConn ::(PS.Connection -> IO a) -> Db m a
  -- Transact ::Sem '[Transaction, Embed IO] a -> Db m a

makeSem ''Db

runDbIO
  :: Members '[Embed IO , Reader RT.DBRuntime] r => Sem (Db ': r) a -> Sem r a
runDbIO = interpret $ \case
  SelectNoTrans sel      -> withRuntimeConnection (`O.runSelect` sel)
  Select        sel      -> withRuntimeTransaction (`O.runSelect` sel)
  -- TransactConn  withConn -> withRuntimeTransaction withConn
  -- Transact      trans'   -> withRuntimeTransaction
    -- $ \conn -> runM $ Polysemy.Reader.runReader conn undefined -- (runTransactionIO trans')

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

-- | Execute a transaction on a single connection from the DB connection pool.
runTransactionEmbed
  :: Members '[Embed IO , Reader RT.DBRuntime] r
  => Sem (Transaction : r) a
  -> (Sem r a -> IO b)
  -> Sem r b
runTransactionEmbed sem runSem =
  let runSemIO conn = runSem . runInputConst conn $ runTransactionWithConn sem
  in  withRuntimeTransaction runSemIO

