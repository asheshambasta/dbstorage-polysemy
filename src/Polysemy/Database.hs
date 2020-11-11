{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
module Polysemy.Database
  ( runDbIO
  , selectNoTrans
  , select
  )
where

-- Everything for Opaleye
import           Data.Profunctor.Product.Default
                                                ( Default )
import           Opaleye                        ( FromFields )
import qualified Opaleye.Select                as Sel
import qualified Opaleye.RunSelect             as Sel
import qualified Database.PostgreSQL.Simple    as PS

-- Within our ecosystem
import qualified Database.Runtime              as RT

-- Polysemy
import           Polysemy
import           Polysemy.Reader

data Db m a where
  -- | Select without transactions: to be used where consistency of reads is not important.
  SelectNoTrans ::Default FromFields sql hask => Sel.Select sql -> Db m [hask]
  -- | Select with transactions.
  Select ::Default FromFields sql hask => Sel.Select sql -> Db m [hask]

makeSem ''Db

runDbIO
  :: Members '[Embed IO , Reader RT.DBRuntime] r => Sem (Db ': r) a -> Sem r a
runDbIO = interpret $ \case
  SelectNoTrans sel -> withRuntimeConnection (`Sel.runSelect` sel)
  Select        sel -> withRuntimeTransaction (`Sel.runSelect` sel)

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
withRuntimeTransaction withConn = do
  rt <- ask
  embed . RT.withConnection rt $ \conn ->
    PS.withTransaction conn $ withConn conn
