{-# language ScopedTypeVariables #-}
{-# language MonadComprehensions #-}
{-# language TupleSections #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

import Text.RawString.QQ
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
import JPL.Core.Builtins

-- * test utils

isRight :: Either a b -> Bool
isRight e = either (const False) (const True) e

(<?>) :: (Testable p) => p -> String -> Property
(<?>) = flip (Test.QuickCheck.counterexample . ("Extra Info: " ++))
infixl 2 <?>

(===>) :: String -> Either FailReason String -> Expectation
expr ===> expect  =  case expect of
  Left err -> eval' 1000000 <$> parseExpr expr `shouldBe` Right (Left err)
  Right res -> eval' 1000000 <$> parseExpr expr `shouldBe` Right <$> parseExpr res

-- * tests

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

      describe "Functions" $ do
        describe "eval" $ do
          it "simple cases (1)" $ do
            "(x? x) 1" ===> Right "1"
            "(1? 2) 1" ===> Right "2"
            "(1? 2) 0" ===> Left ImproperCall

          it "simple cases (2)" $ do
            "(x? (1? 2) x) 0" ===> Left (LogicalError "improper call")

          it "simple cases (3)" $ do
            "(null? 0 | \"one\"? 1) null" ===> Right "0"
            "(null? 0 | \"one\"? 1) \"one\"" ===> Right "1"
            "(null? 0 | \"one\"? 1) \"two\"" ===> Left ImproperCall
            "(null? 0 | \"one\"? 1) \"three\"" ===> Left ImproperCall

          it "sk" $ do
            "(f? g? x? (f x) (g x)) add (add 1) 1" ===> Right "3"
            "(x? y? x) 1 2" ===> Right "1"

      describe "Builtins" $ do
        describe "arith" $ do
          it "add" $ do
            "add 0 0" ===> Right "0"
            "add 1 -1" ===> Right "0"
            "add 1 2" ===> Right "3"
            "add 42 42" ===> Right "84"

          it "recur" $ do
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 0" ===> Right "0"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 1" ===> Right "1"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 2" ===> Right "1"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 3" ===> Right "2"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 4" ===> Right "3"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 5" ===> Right "5"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 6" ===> Right "8"

