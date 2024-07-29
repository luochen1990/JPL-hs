module JPL.Core.Generators where

import Prelude
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Functor
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
divide 0 _ = pure []
divide m 0 = pure (replicate m 0)
divide 1 n = pure [n]
divide 2 n = arbNat n >>= \x -> pure [x, n - x]
divide m n = shuffle =<< repM (m-1) (\(x : xs) -> (xs ++) <$> divide 2 x) [n] where
  repM n f = foldr (>=>) pure (replicate n f)

arbNatSized :: Int -> Gen Int
arbNatSized n = resize n arbitrarySizedNatural

arbSet :: Ord a => Int -> Gen a -> Gen [a]
arbSet n k = vectorOf n k <&> (nub . sort)

arbMap :: Ord a => Int -> Gen a -> Gen b -> Gen [(a, b)]
arbMap n k v = [ zip ks vs | ks <- arbSet n k, vs <- vectorOf (length ks) v]

shrinkSnd :: Arbitrary b => (a, b) -> [(a, b)]
shrinkSnd (x, y) = [ (x, y') | y' <- shrink y]

arbPattern :: Gen Expr
arbPattern = arbitrary `suchThat` isPattern

instance Arbitrary Expr where
  arbitrary = sized tree' where
    tree' 0 = oneof [
      Number <$> elements [0, 1, -1, 42, -42, 3.14, -3.14],
      Text <$> arbKey,
      Boolean <$> arbitrary,
      pure Null,
      Var <$> arbIdent]
    tree' n | n > 0 = oneof [
      tree' 0,
      List <$> (arbNat 3 >>= \m -> divide m (n-1) >>= \ks -> sequence [ tree' k | k <- ks]),
      Dict <$> (arbNat 3 >>= \m -> divide m (n-1) >>= \ks -> sequence [ (,) <$> arbKey <*> tree' k | k <- ks]),
      divide 2 (n-1) >>= \[n1, n2] -> App <$> tree' n1 <*> tree' n2,
      divide 2 (n-1) >>= \[n1, n2] -> Lam <$> resize n1 arbPattern <*> tree' n2,
      divide 2 (n-1) >>= \[n1, n2] -> Alt <$> tree' n1 <*> tree' n2]
  shrink d = case d of
    Number x -> Null : [ Number 1 | x /= 1]
    Text s -> Null : [ Text "a" | s /= "a"]
    Boolean b -> [Null]
    Null -> []
    List xs -> xs ++ [ List xs' | xs' <- shrink xs]
    Dict ps -> map snd ps ++ [ Dict ps' | ps' <- shrinkList shrinkSnd ps]
    App ef ex -> ef : ex : [ App ef' ex' | ef' <- shrink ef, ex' <- shrink ex]
    Lam pat e -> pat : e : [ Lam pat' e' | pat' <- shrink pat, e' <- shrink e]
    Alt ef eg -> ef : eg : [ Alt ef' eg' | ef' <- shrink ef, eg' <- shrink eg]
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
    Lam pat e -> variant 8 . coarbitrary (pat, e)
    Alt ef eg -> variant 9 . coarbitrary (ef, eg)
