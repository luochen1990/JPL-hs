{-# language ScopedTypeVariables #-}
{-# language MonadComprehensions #-}
{-# language TupleSections #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

import Debug.Trace
import qualified Data.Map as M
import Data.List
import Data.Maybe (mapMaybe)
import Control.Exception
import Data.Bytes.Serial
import Data.Bytes.Get
import Data.Bytes.Put
import Test.Hspec hiding (Spec, example)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import JPL.Core.Definitions
import JPL.Core.Functions
import JPL.Core.Parser
import JPL.Core.Generators

isRight :: Either a b -> Bool
isRight e = either (const False) (const True) e

(<?>) :: (Testable p) => p -> String -> Property
(<?>) = flip (Test.QuickCheck.counterexample . ("Extra Info: " ++))
infixl 2 <?>

main :: IO ()
main = hspec $ do
  describe "Arith" $ do
    prop "plus-comm" $
      \(x :: Int) (y :: Int) ->
        collect (x == y) $ x + y === y + x

  describe "JPL" $ do
    describe "Core" $ do
      describe "Parser" $ do
        prop "parseExpr <> show == identity" $
          \(expr :: Expr) ->
            (parseExpr (showLit maxBound expr)) === (Right expr :: Either String Expr)

