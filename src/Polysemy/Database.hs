{-# LANGUAGE
    TemplateHaskell
  , BlockArguments 
  , TypeOperators
  , DataKinds
  , PolyKinds
  , ConstraintKinds
  , TypeApplications
#-}
module Polysemy.Database
  ( -- runDbIO
  -- , selectNoTrans
  -- , select
  -- , transact
  -- , transactConn
  Transaction(..)

  , withRuntimeConnection
  , withRuntimeTransaction

  -- ** Transactional 
  , trSelect
  , trUpdateReturning
  , trInsertManyReturning
  , trDelete
  , trDeleteReporting
  , trInsert
  , trAbortWith
  , runTransactionEmbed
  -- * $commonConstraints
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

import           Control.Exception              ( catch )


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

  TrAbortWith ::IsKnownError e => e -> Transaction m [hask]

  -- todo add others.

makeSem ''Transaction

-- | Run a bulk of operations within a transaction.
prepareTransaction
  :: Members '[Embed IO , Error KnownError] r
  => Sem (Transaction ': r) a
  -> Sem (Input PS.Connection ': r) a
prepareTransaction = reinterpret $ \case

  TrSelect sel -> withInputConn (`O.runSelect` sel)

  TrInsertManyReturning table rows returning' ->
    withInputConn $ \conn -> O.runInsertManyReturning conn table rows returning'

  TrDelete table where' ->
    withInputConn $ \conn -> O.runDelete conn table where'
  TrDeleteReporting id table where' -> withInputConn $ \conn ->
    O.runDelete conn table where' <&> flip replicate id . fromIntegral

  TrUpdateReturning table updateRows where' returning' -> withInputConn
    $ \conn -> O.runUpdateReturning conn table updateRows where' returning'

  TrAbortWith err -> throwKnownError err 

withInputConn
  :: Member (Embed IO) r
  => (PS.Connection -> IO a)
  -> Sem (Input PS.Connection : r) a
withInputConn withConn = input >>= embed . withConn

withRuntimeConnection
  :: Members '[Embed IO , Reader RT.DBRuntime] r
  => (PS.Connection -> IO a)
  -> Sem r a
withRuntimeConnection withConn = do
  rt <- ask
  embed . RT.withConnection rt $ withConn

withRuntimeTransaction
  :: Members '[Embed IO , Reader RT.DBRuntime, Error KnownError] r
  => (PS.Connection -> IO a)
  -> Sem r a
withRuntimeTransaction withConn = do 
  eA <- withRuntimeConnection $ \conn -> withTransaction conn $ try @SomeException (withConn conn)
  either throw pure eA 

-- $commonConstraints

type Transact r = Members '[Embed IO , Reader PS.Connection] r
type Runtime r = Members '[Embed IO , Reader RT.DBRuntime] r

-- | `PS.withTransaction` is too low-level in that it expects exceptions to be thrown in IO to abort transactions.
-- Instead, here, we extend upon that with an IO function that instead returns an Either. 
withTransaction
  :: forall e a . Exception e => PS.Connection -> IO (Either e a) -> IO (Either KnownError a)
withTransaction conn ioEither =
  let ioThrow = ioEither >>= either throwIO (pure . Right)
  in  PS.withTransaction conn ioThrow
        `Control.Exception.catch` (pure . Left . KnownException @e)

-- | Execute a transaction on a single connection from the DB connection pool.
runTransactionEmbed
  :: forall a b r . Members '[Embed IO , Reader RT.DBRuntime , Error KnownError] r
  => Sem (Transaction : r) a
  -> (Sem r a -> IO (Either KnownError b))
  -> Sem r b
runTransactionEmbed sem runSem =
  let runSemIO conn = withTransaction conn . runSem . runInputConst conn $ prepareTransaction sem
  in either throw pure =<< withRuntimeConnection runSemIO 
