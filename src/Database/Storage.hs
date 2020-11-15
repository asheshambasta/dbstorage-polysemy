{-# LANGUAGE TypeFamilies
            , DataKinds
            , TypeOperators
#-}
module Database.Storage
  ( -- $ident
    DBIdentity(..)
  -- $storage
  , DBStorage(..)
  , IdMap
  , mkIdMap
  )
where

import qualified Data.Map                      as M
import qualified Polysemy.Database             as DB
import           Polysemy

-- | A map of all ids and the values.
type IdMap a = M.Map (DBId a) a

mkIdMap :: (DBStorage a, Ord (DBId a), Foldable f) => f a -> IdMap a
mkIdMap = foldl' add' mempty where add' acc a = M.insert (dbId a) a acc

-- $ident Something that has an id and hence can be stored in the database.
class DBIdentity stored where

  -- | The type of the ID 
  type DBId stored :: Type

  -- | Get an ID from a value.
  dbId :: stored -> DBId stored

-- $storage
-- This typeclass dictates the storage related behaviour of its instances.
class (DBIdentity stored) => DBStorage stored where

  -- | Additonal constraints Updates need. 
  type UpdateConstraints stored :: [(Type -> Type) -> Type -> Type]
  type UpdateConstraints stored = '[]

  -- | Additonal constraints Selects need. 
  type SelectConstraints stored :: [(Type -> Type) -> Type -> Type]
  type SelectConstraints stored = '[]

  -- | The available db selects.
  data DBSelect stored
  -- | The available db updates.
  data DBUpdate stored

  -- | Errors, if any, that can be raised by db operations.
  type DBError stored

  {- | Select a list of stored values by their IDs.
     IDs missing from the DB will not be present in the map.
  -}
  selectByIds :: (Foldable f, Functor f, Members (DB.Transaction ': SelectConstraints stored) r) => f (DBId stored) -> Sem r (IdMap stored)

  -- | DBSelect 
  dbSelect :: Members (DB.Transaction ': SelectConstraints stored) r => DBSelect stored -> Sem r (IdMap stored)

  -- | Perform a DB Update.
  dbUpdate :: Members (DB.Transaction ': UpdateConstraints stored) r => DBUpdate stored -> Sem r [DBId stored]





