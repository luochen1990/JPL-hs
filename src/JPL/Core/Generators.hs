{-# language MonadComprehensions #-}
{-# language TupleSections #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

module JPL.Core.Generators where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Monad
import Test.QuickCheck
import JPL.Core.Definitions
import JPL.Core.Functions

arbKey, arbKey' :: Gen String
arbKey = elements ["a", "b", "c", "d", "e"]
arbKey' = arbKey >>= \s -> pure ('_':s)

arbIdent :: Gen String
arbIdent = elements ["x", "y", "z", "u", "v", "w"]

arbFnName :: Gen String
arbFnName = elements ["f", "g", "h"]

arbNat :: Int -> Gen Int
arbNat n = resize n arbitrarySizedNatural

divide :: Int -> Int -> Gen [Int]
divide m _ | m < 1 = complain "`m` should >= 1"
divide m 0 = pure (replicate m 0)
divide 1 n = pure [n]
divide 2 n = arbNat n >>= \x -> pure [x, n - x]
divide m n = repM (m-1) (\(x : xs) -> (xs ++) <$> (divide 2 x)) [] where
  repM n f = foldr (>=>) pure (replicate n f)

arbNatSized :: Int -> Gen Int
arbNatSized n = resize n arbitrarySizedNatural

arbSet :: Ord a => Int -> Gen a -> Gen [a]
arbSet n k = vectorOf n k >>= pure . nub . sort

arbMap :: Ord a => Int -> Gen a -> Gen b -> Gen [(a, b)]
arbMap n k v = [zip ks vs | ks <- arbSet n k, vs <- vectorOf (length ks) v]

shrinkSnd :: Arbitrary b => (a, b) -> [(a, b)]
shrinkSnd (x, y) = [(x, y') | y' <- shrink y]

arbPattern :: Gen Expr
arbPattern = arbitrary `suchThat` isPattern

instance Arbitrary Expr where
  arbitrary = sized tree' where
    tree' 0 = oneof [
      Number <$> elements [0, 1, -1, 42, 3.14],
      Text <$> arbKey,
      Boolean <$> arbitrary,
      pure Null,
      Var <$> arbIdent]
    tree' n | n > 0 = oneof [
      tree' 0,
      List <$> (arbNat 3 >>= \m -> vectorOf m (tree' ((n-1) `div` m))),
      Dict <$> (arbNat 3 >>= \m -> arbMap m arbKey (tree' ((n-1) `div` m))),
      (divide 2 (n-1) >>= \[n1, n2] -> App <$> tree' n1 <*> tree' n2),
      (divide 2 (n-1) >>= \[n1, n2] -> Let <$> arbIdent <*> tree' n1 <*> tree' n2),
      (divide 2 (n-1) >>= \[n1, n2] -> Assume <$> tree' n1 <*> tree' n2),
      (divide 2 (n-1) >>= \[n1, n2] -> Assert <$> tree' n1 <*> tree' n2),
      (divide 2 (n-1) >>= \[n1, n2] -> Lam <$> resize n1 arbPattern <*> tree' n2),
      (divide 2 (n-1) >>= \[n1, n2] -> Alt <$> tree' n1 <*> tree' n2)]
  shrink d = case d of
    Number x -> Null : [Number 1 | x /= 1]
    Text s -> Null : [Text "a" | s /= "a"]
    Boolean b -> [Null]
    Null -> []
    List xs -> xs ++ [List xs' | xs' <- shrink xs]
    Dict ps -> map snd ps ++ [Dict ps' | ps' <- shrinkList shrinkSnd ps]
    App ef ex -> ef : ex : [App ef' ex' | ef' <- shrink ef, ex' <- shrink ex]
    Let k v e -> v : e : [Let k v' e' | v' <- shrink v, e' <- shrink e]
    Assume ep ex -> ep : ex : [Assume ep' ex' | ep' <- shrink ep, ex' <- shrink ex]
    Assert ep ex -> ep : ex : [Assert ep' ex' | ep' <- shrink ep, ex' <- shrink ex]
    Lam pat e -> pat : e : [Lam pat' e' | pat' <- shrink pat, e' <- shrink e]
    Alt ef eg -> ef : eg : [Alt ef' eg' | ef' <- shrink ef, eg' <- shrink eg]
    _ -> [Null]

instance CoArbitrary Expr where
  coarbitrary d = case d of
    Number x -> variant 0 . coarbitrary x
    Text s -> variant 1 . coarbitrary s
    Boolean b -> variant 2 . coarbitrary b
    Null -> variant 3
    List xs -> variant 4 . coarbitrary xs
    Dict ps -> variant 5 . coarbitrary ps
    Var id -> variant 6 . coarbitrary id
    App ef ex -> variant 7 . coarbitrary (ef, ex)
    Let k v e -> variant 8 . coarbitrary (k, v, e)
    Assume p e -> variant 9 . coarbitrary (p, e)
    Assert p e -> variant 10 . coarbitrary (p, e)
    Lam pat e -> variant 12 . coarbitrary (pat, e)
    Alt ef eg -> variant 13 . coarbitrary (ef, eg)

