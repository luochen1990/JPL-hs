{-# language MonadComprehensions #-}
{-# language TupleSections #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

module JPL.Core.Generators where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Test.QuickCheck
import JPL.Core.Definitions
import JPL.Core.Functions

arbKey, arbKey' :: Gen String
arbKey = elements ["a", "b", "c", "d", "e"]
arbKey' = arbKey >>= \s -> pure ('_':s)

arbNat :: Gen Int
arbNat = elements [0, 1, 2]

arbNatSized :: Int -> Gen Int
arbNatSized n = resize n arbitrarySizedNatural

arbSet :: Ord a => Int -> Gen a -> Gen [a]
arbSet n k = vectorOf n k >>= pure . nub . sort

arbMap :: Ord a => Int -> Gen a -> Gen b -> Gen [(a, b)]
arbMap n k v = [zip ks vs | ks <- arbSet n k, vs <- vectorOf (length ks) v]

shrinkSnd :: Arbitrary b => (a, b) -> [(a, b)]
shrinkSnd (x, y) = [(x, y') | y' <- shrink y]

instance Arbitrary Expr where
  arbitrary = sized tree' where
    tree' 0 = oneof [
      Number <$> arbitrary,
      Text <$> arbKey,
      Boolean <$> arbitrary,
      pure Null]
    tree' n | n > 0 = oneof [
      tree' 0,
      List <$> (arbNat >>= \m -> vectorOf m (tree' ((n-1) `div` m))),
      Dict <$> (arbNat >>= \m -> arbMap m arbKey (tree' ((n-1) `div` m)))]
  shrink d = case d of
    List xs -> xs ++ [List xs' | xs' <- shrink xs]
    Dict ps -> map snd ps ++ [Dict ps' | ps' <- shrinkList shrinkSnd ps]
    Number x -> Null : [Number 1 | x /= 1]
    Text s -> Null : [Text "a" | s /= "a"]
    Null -> []
    _ -> [Null]

instance CoArbitrary Expr where
  coarbitrary d = case d of
    Number x -> variant 0 . coarbitrary x
    Text s -> variant 1 . coarbitrary s
    Boolean b -> variant 2 . coarbitrary b
    Null -> variant 3
    List xs -> variant 4 . coarbitrary xs
    Dict ps -> variant 5 . coarbitrary ps

