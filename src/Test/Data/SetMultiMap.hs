{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Data.SetMultiMap where

import Test.Framework
import qualified Data.SetMultiMap as SetMultiMap; import Data.SetMultiMap (SetMultiMap)
import qualified Data.List as List
import qualified Data.Set as Set; import Data.Set (Set)
import Control.Applicative



prop_replacingRemovesOldValueAndInsertsNewOne (mm :: SetMultiMap Char Int) v' = 
  let 
    keys = SetMultiMap.keys mm
    values = SetMultiMap.elems mm
    in
      (not $ null keys) && (not $ null values) && (not $ elem v' values) ==>
      forAll ((,) <$> elements keys <*> elements values) $ \(k, v) -> let
        mm' = SetMultiMap.replace k v v' mm
        s = SetMultiMap.lookup k mm'
        in (not $ Set.member v s) .&&. (Set.member v' s)

prop_toFromList (mm :: SetMultiMap Char Int) = l == l' where
  l = SetMultiMap.toAscList mm
  l' = SetMultiMap.toAscList $ SetMultiMap.fromList l

prop_toAscListIsInFactAscending (mm :: SetMultiMap Int Double) = 
  l == List.sort l where
    l = SetMultiMap.toAscList mm



-- | Useful for generating a map which is a result of arbitrary modifications.
newtype Modification k v = Modification (SetMultiMap k v -> SetMultiMap k v)

instance 
  ( Arbitrary k, Arbitrary v, Ord k, Ord v ) => 
  Arbitrary (Modification k v) 
  where
    arbitrary = 
      fmap Modification $ oneof [insert, delete, deleteAll, replace, alterF]
      where
        insert = promote $ \mm -> do
          k <- arbitrary
          v <- arbitrary
          return $ SetMultiMap.insert k v mm
        delete = promote $ \mm -> do
          if SetMultiMap.null mm
            then return mm
            else do
              k <- elements $ SetMultiMap.keys mm
              v <- elements $ SetMultiMap.elems mm
              return $ SetMultiMap.delete k v mm
        deleteAll = promote $ \mm -> do
          if SetMultiMap.null mm
            then return mm
            else do
              k <- elements $ SetMultiMap.keys mm
              return $ SetMultiMap.deleteAll k mm
        replace = promote $ \mm -> do
          if SetMultiMap.null mm
            then return mm
            else do
              k <- elements $ SetMultiMap.keys mm
              v <- elements $ SetMultiMap.elems mm
              v' <- arbitrary
              return $ SetMultiMap.replace k v v' mm
        alterF = promote $ \mm -> do
          if SetMultiMap.null mm
            then return mm
            else do
              k <- elements $ SetMultiMap.keys mm
              f <- do
                l <- arbitrary
                return $ \_ -> (undefined, Set.fromList l) 
              return $ snd $ SetMultiMap.alterF k f mm


-- | Simulates a map which is a result of sequentially applied arbitrary 
-- modifications, thus being closest to the actual use-cases and covering most
-- modification functions.
instance 
  ( Arbitrary k, Arbitrary v, Ord k, Ord v ) => 
  Arbitrary (SetMultiMap k v) 
  where
    arbitrary = do
      mods <- listOf arbitrary
      return $ foldr ($) SetMultiMap.empty $ map (\(Modification f) -> f) mods



