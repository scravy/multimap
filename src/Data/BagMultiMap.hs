-- |
-- Module       : Data.BagMultiMap
-- Copyright    : (c) Julian Fleischer 2013
-- License      : MIT (See LICENSE file in cabal package)
--
-- Maintainer   : julian.fleischer@fu-berlin.de
-- Portability  : non-portable (DeriveDataTypeable)
-- 
-- A very simple MultiMap, based on 'Data.Map.Map' from the containers package.
module Data.BagMultiMap (

    -- * MultiMap type
    BagMultiMap,

    -- * Query
    null,
    size,
    numKeys,
    numValues,

    member,
    notMember,
    lookup,

    -- * Operators
    (!),

    -- * Construction
    empty,
    
    -- ** Insertion
    insert,

    -- ** Delete
    delete,
    deleteAll,

    -- * Traversal
    map,
    mapKeys,
    mapWithKey,
    
    -- * Folds
    foldr,
    foldl,
    foldrWithKey,
    foldlWithKey,

    -- * Conversion
    elems,
    keys,
    keysSet,
    assocs,

    toMap,
    toMapOfSets,
    toList,
    fromList,
    fromMap,
    
    -- * Min/Max
    findMin,
    findMax,
    findMinWithValues,
    findMaxWithValues

  ) where

import Prelude hiding (lookup, map, null, foldr, foldl)
import qualified Prelude as P

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Word
import Data.Data


-- | A MultiMap with keys @k@ and values @v@.
--
-- A key can have multiple values (but not zero).
-- The same value can be added multiple times (thus no
-- constraints are ever imposed on @v@).
--
-- Internally this is simply a @Map k [v]@.
-- See 'toMap' for accessing the underlying 'Map'.
newtype BagMultiMap k v = BagMultiMap (Word32, Word32, Map k [v])
    deriving (Data, Typeable)


null :: BagMultiMap k a -> Bool
-- ^ /O(1)./ Check whether the multimap is empty or not.
null (BagMultiMap (_, _, m)) = Map.null m


size :: BagMultiMap k a -> Int
-- ^ /O(1)./ The number of elements in the multimap.
size (BagMultiMap (_, size, _)) = fromIntegral size


numKeys :: BagMultiMap k a -> Word32
-- ^ /O(1)./ The number of keys in the multimap.
-- 
-- As this is a multimap, the number of keys is not
-- necessarily equal to the number of values.
numKeys (BagMultiMap (nk, _, _)) = nk


numValues :: BagMultiMap k a -> Word32
-- ^ /O(1)./ The number of values in the multimap.
--
-- As this is a multimap, the number of keys is not
-- necessarily equal to the number of values.
numValues (BagMultiMap (_, nv, _)) = nv


notMember, member :: Ord k => BagMultiMap k a -> k -> Bool
-- | /O(log n)./ Is the key a member of the multimap?
member (BagMultiMap (_, _, map)) key = Map.member key map
-- | /O(log n)./ Is the key not a member of the multimap?
notMember key = not . member key


(!) :: Ord k => BagMultiMap k a -> k -> [a]
-- ^ As @flip lookup@
(!) = flip lookup


lookup :: Ord k => k -> BagMultiMap k a -> [a]
-- ^ /O(log n)./ Lookup the value at a key in the map.
--
-- The function will return the corrsponding values as a List, or the
-- empty list if no values are associated witht the given key.
lookup key (BagMultiMap (_, _, map)) = maybe [] id (Map.lookup key map)


empty :: BagMultiMap k a
-- ^ /O(1)./ The empty multimap.
empty = BagMultiMap (0, 0, Map.empty)


insert :: Ord k => k -> a -> BagMultiMap k a -> BagMultiMap k a
-- ^ /O(log n)./ Insert a new key and value in the map.
insert k v (BagMultiMap (nk, nv, map))
    | Map.member k map = BagMultiMap (nk, succ nv, Map.insert k (v : map Map.! k) map)
    | otherwise = BagMultiMap (succ nk, succ nv, Map.insert k [v] map)


delete :: (Ord k, Ord e) => k -> e -> BagMultiMap k a -> BagMultiMap k a
-- ^ Delete a key/value pair from the map.
delete = undefined


deleteAll :: Ord k => k -> BagMultiMap k a -> BagMultiMap k a
-- ^ /O(log n)./ Delete a key and all its values from the map.
deleteAll k m@(BagMultiMap (nk, nv, map)) = case Map.lookup k map of
    Just v -> BagMultiMap (pred nk, nv - fromIntegral (length v), Map.delete k map)
    _      -> m


map :: (a -> b) -> BagMultiMap k a -> BagMultiMap k b
-- ^ Map a function over all values in the map.
map f (BagMultiMap (nk, nv, map)) = BagMultiMap (nk, nv, Map.map (P.map f) map)


mapKeys :: Ord k2 => (k1 -> k2) -> BagMultiMap k1 a -> BagMultiMap k2 a
-- ^ mapKeys f s is the multimap obtained by applying f to each key of s.
mapKeys f (BagMultiMap (nk, nv, map)) = BagMultiMap (nk, nv, Map.mapKeys f map)


mapWithKey :: (k -> a -> b) -> BagMultiMap k a -> BagMultiMap k b
-- ^ Map a function over all key/value pairs in the map.
mapWithKey f (BagMultiMap (nk, nv, map))
  = BagMultiMap (nk, nv, Map.mapWithKey (\k -> P.map (f k)) map)


foldr :: (a -> b -> b) -> b -> BagMultiMap k a -> b
-- ^ Fold the values in the map using the given right-associative binary operator.
foldr f e = P.foldr f e . concat . elems


foldl :: (a -> b -> a) -> a -> BagMultiMap k b -> a
-- ^  Fold the values in the map using the given left-associative binary operator.
foldl f e = P.foldl f e . concat . elems


foldrWithKey :: (k -> a -> b -> b) -> b -> BagMultiMap k a -> b
-- ^ /O(n)./ Fold the keys and values in the map using the given right-associative
-- binary operator, taking into account not only the value but also the key.
foldrWithKey f e = P.foldr (uncurry f) e . toList


foldlWithKey :: (a -> k -> b -> a) -> a -> BagMultiMap k b -> a
-- ^ /O(n)./ Fold the keys and values in the map using the given left-associative
-- binary operator, taking into account not only the value but also the key.
foldlWithKey f e = P.foldl (\a (k,v) -> f a k v) e . toList


elems :: BagMultiMap k a -> [[a]]
-- ^ /O(n)./ Return all elements of the multimap in the
-- ascending order of their keys.
--
-- A list of lists is returned since a key can have
-- multiple values. Use 'concat' to flatten.
elems (BagMultiMap (_, _, map)) = Map.elems map


keys :: BagMultiMap k a -> [k]
-- ^ /O(n)./ Return all keys of the multimap in ascending order.
keys (BagMultiMap (_, _, map)) = Map.keys map


keysSet :: BagMultiMap k a -> Set k
-- ^ /O(n)./ The set of all keys of the multimap.
keysSet (BagMultiMap (_, _, map)) = Map.keysSet map


assocs :: BagMultiMap k a -> [(k, [a])]
-- ^ /O(n)./ Return all key/value pairs in the multimap
-- in ascending key order.
assocs (BagMultiMap (_, _, map)) = Map.assocs map


toMap :: BagMultiMap k a -> Map k [a]
-- ^ /O(1)./ Return the map of lists.
toMap (BagMultiMap (_, _, theUnderlyingMap)) = theUnderlyingMap


toMapOfSets :: Ord a => BagMultiMap k a -> Map k (Set a)
-- ^ /O(k*m*log m) where k is the number of keys and m the
-- maximum number of elements associated with a single key/
toMapOfSets (BagMultiMap (_, _, map)) = Map.map Set.fromList map


toList :: BagMultiMap k a -> [(k, a)]
-- ^ Return a flattened list of key/value pairs.
toList (BagMultiMap (_, _, map))
  = concat $ Map.elems $ Map.mapWithKey (\k -> zip (repeat k)) map


fromList :: Ord k => [(k, a)] -> BagMultiMap k a
-- ^ /O(n*log n)/ Create a multimap from a list of key/value pairs.
--
-- > fromList xs == foldr (uncurry insert) empty
fromList = P.foldr (uncurry insert) empty


fromMap :: Map k [a] -> BagMultiMap k a
-- ^ Turns a map of lists into a multimap.
fromMap map = BagMultiMap (numKeys, numValues, map)
  where
    numKeys   = fromIntegral $ Map.size map
    numValues = fromIntegral $ Map.foldr (\v s -> length v + s) 0 map


findMin :: BagMultiMap k a -> Maybe k
-- ^ /O(log n)/ Find the minimal key of the multimap.
findMin (BagMultiMap (_, _, map))
    | Map.null map = Nothing
    | otherwise    = Just $ fst $ Map.findMin map


findMax :: BagMultiMap k a -> Maybe k
-- ^ /O(log n)/ Find the maximal key of the multimap.
findMax (BagMultiMap (_, _, map))
    | Map.null map = Nothing
    | otherwise    = Just $ fst $ Map.findMax map


findMinWithValues :: BagMultiMap k a -> Maybe (k, [a])
-- ^ /O(log n)/ Find the minimal key and the values associated with it.
findMinWithValues (BagMultiMap (_, _, map))
    | Map.null map = Nothing
    | otherwise    = Just $ Map.findMin map


findMaxWithValues :: BagMultiMap k a -> Maybe (k, [a])
-- ^ /O(log n)/ Find the maximal key and the values associated with it.
findMaxWithValues (BagMultiMap (_, _, map))
    | Map.null map = Nothing
    | otherwise    = Just $ Map.findMax map




