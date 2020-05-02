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
import Data.Bifunctor
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

ignoreErrorDetail :: FailReason -> FailReason
ignoreErrorDetail e = case e of
  OutOfFuel -> OutOfFuel
  Complain _ -> Complain ""
  ImproperCall _ -> ImproperCall ""
  LogicalError _ -> LogicalError ""

(===>) :: String -> Either FailReason String -> Expectation
expr ===> expect  =  case expect of
  Left err -> fmap (first ignoreErrorDetail) (eval' 1000000 <$> parseExpr expr) `shouldBe` Right (Left (ignoreErrorDetail err))
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
          it "beta-reduce" $ do
            "(x? x) 1" ===> Right "1"
            "(1? 2) 1" ===> Right "2"

          it "name-shadowing" $ do
            "(x? (x? x)) 1 2" ===> Right "2"

          it "partial-function" $ do
            "(1? 2) 0" ===> Left (ImproperCall "")
            "(x? (1? 2) x) 0" ===> Left (LogicalError "improper call")

          it "alt" $ do
            "(null? 0 | \"one\"? 1) null" ===> Right "0"
            "(null? 0 | \"one\"? 1) \"one\"" ===> Right "1"
            "(null? 0 | \"one\"? 1) \"two\"" ===> Left (ImproperCall "")
            "(null? 0 | \"one\"? 1) \"three\"" ===> Left (ImproperCall "")

          it "sk" $ do
            "(f? g? x? (f x) (g x)) add (add 1) 1" ===> Right "3"
            "(x? y? x) 1 2" ===> Right "1"

      describe "Builtins" $ do
        describe "core" $ do
          it "let" $ do
            "let x 1 x" ===> Right "1"
            "let [x, y] [1, 2]; y" ===> Right "2"

          it "assume" $ do
            "1 @ (x? assume (isNumber x); x)" ===> Right "1"
            "1 @ (x? assume (isNumber x); x | _? 0) " ===> Right "1"
            "null @ (x? assume (isNumber x); x) " ===> Left (ImproperCall "")
            "null @ (x? assume (isNumber x); x | _? 0) " ===> Right "0"

        describe "arith" $ do
          it "add" $ do
            "add 0 0" ===> Right "0"
            "add 1 -1" ===> Right "0"
            "add 1 2" ===> Right "3"
            "add 42 42" ===> Right "84"

          it "sub" $ do
            "sub 0 0" ===> Right "0"
            "sub 1 1" ===> Right "0"
            "sub -1 -1" ===> Right "0"
            "sub 2 1" ===> Right "-1"
            "sub 1 2" ===> Right "1"
            "1 @sub 2" ===> Right "-1"
            "2 @sub 1" ===> Right "1"

          it "negate" $ do
            "negate 0" ===> Right "0"
            "negate 1" ===> Right "-1"
            "negate -1" ===> Right "1"
            "1 @negate" ===> Right "-1"

          it "div" $ do
            "div 1 1" ===> Right "1"
            "div 0 1" ===> Left (ImproperCall "")
            "1 @div 0" ===> Left (ImproperCall "")
            "0 @div 1" ===> Right "0"

        describe "control" $ do
          it "if" $ do
            "if (1 @gt 0) 1 0" ===> Right "1"
            "if (1 @lt 0) 1 0" ===> Right "0"
            "if null 1 0" ===> Left (ImproperCall "")

          it "recur" $ do
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 0" ===> Right "0"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 1" ===> Right "1"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 2" ===> Right "1"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 3" ===> Right "2"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 4" ===> Right "3"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 5" ===> Right "5"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 6" ===> Right "8"
            "recur (fib? n? (true? n | false? add (fib (sub 1 n)) (fib (sub 2 n))) (lt 2 n)) 100" ===> Left OutOfFuel

