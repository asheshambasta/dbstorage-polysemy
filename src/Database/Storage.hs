{-# LANGUAGE TypeFamilies #-}
module Database.Storage
  ( DBIdentity(..)
  , IdMap
  )
where

import           Polysemy.Reader
import           Polysemy

-- | A map of all ids and the values.
type IdMap a = Map (DBId a) a

{- | Something that has an id and hence can be stored in the database.
-}
class DBIdentity stored where

  -- | The type of the ID 
  type DBId stored :: Type

  -- | Get an ID from a value.
  dbId :: stored -> DBId stored

class (DBIdentity stored) => DBStorage stored where

  -- | The available db updates.
  data DBUpdate stored

  -- | Errors, if any, that can be raised by db operations.
  type DBError stored

  {- | Select a list of stored values by their IDs.
     IDs missing from the DB will not be present in the map.
  -}
  selectByIds :: (Foldable f, Functor f) => f (DBId stored) -> m (IdMap stored)



