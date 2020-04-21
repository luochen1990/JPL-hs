{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}
{-# language LambdaCase #-}

module JPL.Core.Builtins where

import Debug.Trace
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Either
import Control.Exception
import JPL.Core.Definitions
import JPL.Core.Functions
import JPL.Core.Parser

-- builtin tools

term :: String -> Either NativeFn Expr
term s = Right $ either (error . show) id (parseExpr s)

nativeFn :: Int -> ([Expr] -> Eval Expr) -> Either NativeFn Expr
nativeFn ary core = Left . NativeFn $ \env arg -> do
    arg' <- evalM env arg
    traceM $ "arg': " ++ show arg'
    traceM $ "env: " ++ show (env `M.difference` builtins)
    case arg' of
        (List args) -> if length args == ary then (sequence . map (evalM env) $ args) >>= core else yieldFail ImproperCall
        _ -> yieldFail ImproperCall

-- builtins

builtins :: Env
builtins = M.fromList [
    ("add#", nativeFn 2 $ \case [Number x, Number y] -> yieldSucc (Number (x + y)); _ -> yieldFail ImproperCall),
    ("add", term "x? y? add# [x, y]"),

    ("sub#", nativeFn 2 $ \case [Number x, Number y] -> yieldSucc (Number (x - y)); _ -> yieldFail ImproperCall),
    ("sub", term "x? y? sub# [x, y]"),

    ("mul#", nativeFn 2 $ \case [Number x, Number y] -> yieldSucc (Number (x * y)); _ -> yieldFail ImproperCall),
    ("mul", term "x? y? mul# [x, y]"),

    ("div#", nativeFn 2 $ \case [Number x, Number y] -> if y == 0 then yieldFail ImproperCall else yieldSucc (Number (x / y)); _ -> yieldFail ImproperCall),
    ("div", term "x? y? div# [y, x]"),

    ("exactDiv#", nativeFn 2 $ \case [Number x, Number y] -> if y == 0 then yieldFail ImproperCall else yieldSucc (Number (fromIntegral $ floor (x / y))); _ -> yieldFail ImproperCall),
    ("exactDiv", term "x? y? exactDiv# [y, x]"),

    ("mod#", nativeFn 2 $ \case [Number x, Number y] -> if y == 0 then yieldFail ImproperCall else yieldSucc (Number (fromIntegral $ floor x `mod` floor y)); _ -> yieldFail ImproperCall),
    ("mod", term "x? y? mod# [y, x]")]

