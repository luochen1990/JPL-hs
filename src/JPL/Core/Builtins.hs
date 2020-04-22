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
nativeFn arity core = Left . NativeFn $ \env arg -> do
    arg' <- evalM env arg
    --traceM $ "arg': " ++ show arg'
    --traceM $ "env: " ++ show (env `M.difference` builtins)
    case arg' of
        (List args) -> if length args == arity then (sequence . map (evalM env) $ args) >>= core else yieldFail ImproperCall
        _ -> yieldFail ImproperCall

-- builtins

builtins :: Env
builtins = M.fromList [
    ("add#", nativeFn 2 $ \case [Number x, Number y] -> yieldSucc (Number (x + y)); _ -> yieldFail ImproperCall),
    ("add", term "x? y? add# [y, x]"),

    ("sub#", nativeFn 2 $ \case [Number x, Number y] -> yieldSucc (Number (x - y)); _ -> yieldFail ImproperCall),
    ("sub", term "x? y? sub# [y, x]"),

    ("mul#", nativeFn 2 $ \case [Number x, Number y] -> yieldSucc (Number (x * y)); _ -> yieldFail ImproperCall),
    ("mul", term "x? y? mul# [y, x]"),

    ("div#", nativeFn 2 $ \case [Number x, Number y] -> if y == 0 then yieldFail ImproperCall else yieldSucc (Number (x / y)); _ -> yieldFail ImproperCall),
    ("div", term "x? y? div# [y, x]"),

    ("exactDiv#", nativeFn 2 $ \case [Number x, Number y] -> if y == 0 then yieldFail ImproperCall else yieldSucc (Number (fromIntegral $ floor (x / y))); _ -> yieldFail ImproperCall),
    ("exactDiv", term "x? y? exactDiv# [y, x]"),

    ("mod#", nativeFn 2 $ \case [Number x, Number y] -> if y == 0 then yieldFail ImproperCall else yieldSucc (Number (fromIntegral $ floor x `mod` floor y)); _ -> yieldFail ImproperCall),
    ("mod", term "x? y? mod# [y, x]"),

    ("eq#", nativeFn 2 $ \case
        [Null, Null] -> yieldSucc (Boolean True)
        [Number x, Number y] -> yieldSucc (Boolean (x == y))
        [Text x, Text y] -> yieldSucc (Boolean (x == y))
        [Boolean x, Boolean y] -> yieldSucc (Boolean (x == y))
        --[List xs, List ys] -> error ("TODO")
        --[Dict ps, Dict qs] -> error ("TODO")
        _ -> yieldFail ImproperCall),
    ("eq", term "x? y? eq# [y, x]"),

    ("neq", term "x? y? not (eq# [y, x])"),

    ("lt#", nativeFn 2 $ \case [Number x, Number y] -> yieldSucc (Boolean (x < y)); _ -> yieldFail ImproperCall),
    ("lt", term "x? y? lt# [y, x]"),

    ("le#", nativeFn 2 $ \case [Number x, Number y] -> yieldSucc (Boolean (x <= y)); _ -> yieldFail ImproperCall),
    ("le", term "x? y? le# [y, x]"),

    ("gt#", nativeFn 2 $ \case [Number x, Number y] -> yieldSucc (Boolean (x > y)); _ -> yieldFail ImproperCall),
    ("gt", term "x? y? gt# [y, x]"),

    ("ge#", nativeFn 2 $ \case [Number x, Number y] -> yieldSucc (Boolean (x >= y)); _ -> yieldFail ImproperCall),
    ("ge", term "x? y? ge# [y, x]")]

    --("fix#", Left . NativeFn $ \env arg -> do
    --    arg' <- evalM env arg
    --    traceM $ "arg': " ++ show arg'
    --    traceM $ "env: " ++ show (env `M.difference` builtins)
    --    case arg' of
    --        (List args) -> if length args == arity then (sequence . map (evalM env) $ args) >>= core else yieldFail ImproperCall
    --        _ -> yieldFail ImproperCall

    --("fix", term "x? y? mod# [y, x]")]

