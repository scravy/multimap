-- Module       : Data.SetMultiMap
-- Copyright    : (c) Julian Fleischer 2013
-- License      : MIT (See LICENSE file in cabal package)
--
-- Maintainer   : julian.fleischer@fu-berlin.de
-- Portability  : non-portable (DeriveDataTypeable)
--
-- A MultiMap allows the association of multiple values with a single key,
-- but there are no duplicates per key.
module Data.SetMultiMap (

    -- * MultiMap type
    SetMultiMap,

    -- * Querying
    null,
    size,
    keysSize,
    member,
    notMember,
    lookup,
    (!),

    -- ** Modification
    insert,
    replace,
    delete,
    deleteAll,
    alterF,

    -- * Traversal
    map,

    -- * Construction
    empty,
    fromList,

    -- * Conversion
    elems,
    keys,
    toAscList,
    toMap

  ) where


import Prelude hiding (lookup, map, null, foldr, foldl)
import qualified Prelude as P

import qualified Data.List as List

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Data


-- | A `Set`-based MultiMap with keys @k@ and values @v@.
newtype SetMultiMap k v 
  = SetMultiMap { 
      -- | /O(1)./ Return the underlying map of sets.
      toMap :: Map k (Set v) 
    }
  deriving (Data, Typeable, Eq)

instance (Show k, Show v) => Show (SetMultiMap k v) where
  show m = "fromList " ++ show (toAscList m)


null :: SetMultiMap k a -> Bool
-- ^ /O(1)./ Check whether the multimap is empty or not.
null = Map.null . toMap


size :: SetMultiMap k a -> Int
-- ^ /O(n)./ The number of elements in the multimap.
size = length . toAscList


keysSize :: SetMultiMap k a -> Int
-- ^ /O(n)./ The number of keys in the multimap.
--
-- As this is a multimap, the number of keys is not
-- necessarily equal to the number of values.
keysSize = Map.size . toMap


notMember, member :: Ord k => k -> SetMultiMap k a -> Bool
-- | /O(log n)./ Is the key a member of the multimap?
member k = Map.member k . toMap
-- | /O(log n)./ Is the key not a member of the multimap?
notMember key = not . member key


(!) :: Ord k => SetMultiMap k a -> k -> Set a
-- ^ @flip `lookup`@
(!) = flip lookup


lookup :: Ord k => k -> SetMultiMap k a -> Set a
-- ^ /O(log n)./ Lookup the value at a key in the map.
--
-- The function will return the corresponding values as a Set, or an empty Set if no values are associated with the given key.
lookup k = maybe Set.empty id . Map.lookup k . toMap


empty :: SetMultiMap k a
-- ^ /O(1)./ The empty multimap.
empty = SetMultiMap Map.empty


insert :: (Ord k, Ord a) => k -> a -> SetMultiMap k a -> SetMultiMap k a
-- ^ /O(log n)./ Insert a new key and value in the map.
insert k v = SetMultiMap . Map.insertWith Set.union k (Set.singleton v) . toMap


delete :: (Ord k, Ord v) => k -> v -> SetMultiMap k v -> SetMultiMap k v
-- ^ /O(log n)./ Delete a single key/value pair from the map.
delete k v = SetMultiMap . Map.update update k . toMap where
  update set = newSetM where
    newSetM = if Set.null newSet then Nothing else Just newSet where
      newSet = Set.delete v set


deleteAll :: Ord k => k -> SetMultiMap k a -> SetMultiMap k a
-- ^ /O(log n)./ Delete a key and all its values from the map.
deleteAll k = SetMultiMap . Map.delete k . toMap


-- | /O(log n)./ Replace a value at a given key with a new value.
replace 
  :: (Ord k, Ord v) 
  => k 
  -> v 
  -- ^ Value to replace
  -> v 
  -- ^ New value
  -> SetMultiMap k v -> SetMultiMap k v
replace k v v' (SetMultiMap m) = 
  SetMultiMap $ toResult $ 
  Map.insertLookupWithKey insertLookup k (Set.singleton v') $ m
  where
    insertLookup k new old = Set.union new $ Set.delete v old
    toResult (Just _, m') = m'
    toResult _ = m


map :: (Ord a, Ord b) => (a -> b) -> SetMultiMap k a -> SetMultiMap k b
-- ^ /O(n)./ Map a function over all values in the map.
map f = SetMultiMap . Map.map (Set.map f) . toMap


elems :: SetMultiMap k a -> [a]
-- ^ /O(n)./ Return all elements of the multimap in the
-- ascending order of their keys.
elems = List.map snd . toAscList


keys :: SetMultiMap k a -> [k]
-- ^ /O(n)./ Return all keys of the multimap in ascending order.
keys = List.map fst . toAscList


-- | /O(n)./ Convert to an ascending list of key-value pairs.
toAscList :: SetMultiMap k e -> [(k, e)]
toAscList = Map.foldrWithKey fu [] . toMap where
  fu ke elSe li = li' ++ li where
    li' = Set.foldr (\el li -> (ke, el) : li) [] elSe


-- | /O(n)./ Create from a list of key-value pairs.
fromList :: (Ord e, Ord k) => [(k, e)] -> SetMultiMap k e
fromList = List.foldr (uncurry insert) empty


-- | /O(2 * log n)./ Alter a set of values matched by a key with a functor-function. Allows you to extract and process the matched value, while updating it.
-- 
-- This is the most general modification function. All kinds of modification operations can be defined in terms of it.
-- 
-- You can consider this function to have the following signature: 
-- 
-- > k -> (Set e -> (r, Set e)) -> SetMultiMap k e -> (r, SetMultiMap k e)
-- 
-- In fact it is a strictly more general version of it.
alterF 
  :: (Ord k, Ord e, Functor f)
  => k 
  -> (Set e -> f (Set e)) 
  -- ^ A function from a set of matching values to a functor on a new set of values. If the returned set is empty the key gets removed from the map. The input values, which aren't present in the output set, get deleted. The output values, which aren't present in the input set, get inserted.
  -> (SetMultiMap k e -> f (SetMultiMap k e))
alterF k f m = let
  items = lookup k m
  items'F = f items
  in fmap update items'F where
    update items' = SetMultiMap $ if Set.null items' 
      then Map.delete k $ toMap m
      else Map.insert k items' $ toMap m
