{-# LANGUAGE TypeFamilies
            , DataKinds
            , TypeOperators
#-}
module Database.Storage
  ( DBIdentity(..)
  , DBStorage(..)
  , IdMap
  )
where

import qualified Polysemy.Database             as DB
import           Polysemy

-- | A map of all ids and the values.
type IdMap a = Map (DBId a) a

{- | Something that has an id and hence can be stored in the database.
-}
class DBIdentity stored where

  -- | Additonal constraints Updates need. 
  type UpdateConstraints stored :: [(Type -> Type) -> Type -> Type]
  type UpdateConstraints stored = '[]

  -- | Additonal constraints Selects need. 
  type SelectConstraints stored :: [(Type -> Type) -> Type -> Type]
  type SelectConstraints stored = '[]

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
  selectByIds :: (Foldable f, Functor f, Members (DB.Transaction ': SelectConstraints stored) r) => f (DBId stored) -> Sem r (IdMap stored)

  -- | Perform a DB Update.
  dbUpdate :: Members (DB.Transaction ': UpdateConstraints stored) r => DBUpdate stored -> Sem r [DBId stored]





