{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE TemplateHaskell    #-}
module Database.Conf
  ( DBConf(..)
  , DBConnStr(..)
  , dbPoolConf
  , dbConnStr
  , createPoolDB
  , connectSafe
  -- * Parsers
  , dbConfP
  -- * Re-exports 
  , module Resource
  )
where

import qualified System.Environment            as Env
import qualified Database.PostgreSQL.Simple    as PS
import           Database.Conf.Resource        as Resource
import           Options.Applicative
import           Database.Lib.Redact            ( redactPasswordEquals )
import qualified Data.Text                     as T

-- imported for manual implementations of Show and IsString 
import qualified Data.String                    ( IsString(..)
                                                , String
                                                )
import qualified GHC.Show

import           Data.Default.Class             ( Default(..) )
import           Control.Lens                   ( makeLenses )

-- | A configuration for DB connection strings. 
data DBConnStr = StaticConnStr ByteString
               -- | A Static environment variable connection string is read once at startup time.
               -- From then on, all resources in the pool are created with the same connection string value.
               | StaticEnv Text
               -- | A Dynamic environment variable connection string is read each time a new resource (connection) is created
               -- in the pool
               | DynamicEnv Text
               deriving Eq

-- | This ensures backwards compatibility. For places in which we're using strings for dbconfs;
-- we can continue to do that with minimal changes. It is also convenient.
instance IsString DBConnStr where
  fromString = StaticConnStr . encodeUtf8 . T.pack

instance Default DBConnStr where
  def = StaticEnv "DB_CONN_STR"

instance Show DBConnStr where
  show = \case
    -- we want to redact data.
    StaticConnStr (T.unpack . redactPasswordEquals . decodeUtf8 -> str') ->
      "StaticConnStr " <> str'
    StaticEnv  e -> "StaticEnv " <> T.unpack e
    DynamicEnv e -> "DynamicEnv " <> T.unpack e


data DBConf = DBConf
  { _dbPoolConf :: Resource.PoolConf
  , _dbConnStr  :: DBConnStr
  }
  deriving (Eq, Show)

-- | Simple parser
dbConfP :: Parser DBConf
dbConfP = do
  _dbConnStr  <- parseConnStr
  _dbPoolConf <- Resource.poolConfP "db"
  pure DBConf { .. }
 where
  parseConnStr = staticString <|> staticEnvVar <|> dynamicEnvVar
  staticString = StaticConnStr <$> strOption
    (long "db-conn-str" <> short 'D' <> help "DB connection string" <> metavar
      "DB_CONN_STRING"
    )
  staticEnvVar = StaticEnv <$> strOption
    (  long "db-conn-str-env-static"
    <> help "DB connection string env. variable (read only at startup)"
    <> metavar "ENV"
    )
  dynamicEnvVar = DynamicEnv <$> strOption
    (  long "db-conn-str-env-dynamic"
    <> help
         "DB connection string env. variable (read each time a new connection is made to the pool)"
    <> metavar "ENV"
    )

makeLenses ''DBConf

-- * Creating pools.

-- | Create a pool of DB conns. using a given `DBConnStr` & `PoolConf`
createPoolDB
  :: DBConnStr -> Resource.PoolConf -> IO (Resource.Pool PS.Connection)
createPoolDB connStr poolConf = case connStr of
  StaticConnStr s -> mkPoolWithStr s
  -- for static env. vars, read the env. var, and bind it in the create new connection closure.
  StaticEnv     e -> lookupEnv e >>= onFound mkPoolWithStr
  -- for dynamic environment variables, we want the connect function to look up the environment variable
  -- each time a new connection is made.
  DynamicEnv e ->
    let newConn = lookupEnv e >>= onFound (connectSafe errMsg)
    in  Resource.createPoolFromConf poolConf newConn PS.close
 where
  mkPoolWithStr s =
    Resource.createPoolFromConf poolConf (connectSafe errMsg s) PS.close
  onFound :: (ByteString -> IO b) -> Maybe Data.String.String -> IO b
  onFound f = maybe (envVarNotFound connStr) (f . encodeUtf8 . T.pack)
  lookupEnv = Env.lookupEnv . T.unpack
  errMsg    = show connStr <> ": "

connectSafe :: Text -> ByteString -> IO PS.Connection
connectSafe msg str' = try (PS.connectPostgreSQL str') >>= either throwDB pure
 where
  throwDB =
    throwIO . DBConnectionFailed . mappend connInfoMsg . show @SomeException
  connInfoMsg =
    "[Conn. String: "
      <> redactPasswordEquals (decodeUtf8 str')
      <> " Msg: "
      <> msg
      <> "], Database.PostgreSQL.Simple: "

-- | Custom exceptions on DB connection issues for better debugging.
data DBError = DBEnvVarNotFound Text
             | DBConnectionFailed Text
             deriving Show
             deriving anyclass Exception

envVarNotFound :: DBConnStr -> IO a
envVarNotFound =
  throwIO
    . DBEnvVarNotFound
    . mappend
        "[FATAL] Environment variable not found when trying to create connections for the pool: "
    . show
