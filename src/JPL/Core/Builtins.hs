{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}
{-# language LambdaCase #-}
{-# language BlockArguments #-}

module JPL.Core.Builtins (natives, builtins, eval') where

import Debug.Trace
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Array as A
import Data.Array (Array)
import Data.List
import Data.Maybe
import Data.Either
import Control.Exception
import JPL.Core.Definitions
import JPL.Core.Functions
import JPL.Core.Parser

-- * builtin tools

term :: Ident -> String -> (Ident, Either NativeCode Expr)
term name s = (name, Right $ either (error . show) id (parseExpr s))

macro :: Ident -> Int -> ([Expr] -> Eval Expr) -> (Ident, Either NativeCode Expr)
macro name arity core = (name, Left (NativeCode name arity body)) where
    body = \args -> do
        --traceM $ "args: " ++ show args
        --traceM $ "env: " ++ show (env `M.difference` builtins)
        if length args == arity then core args else yieldFail ImproperCall

nativeFn :: Ident -> Int -> ([Expr] -> Eval Expr) -> (Ident, Either NativeCode Expr)
nativeFn name arity core = (name, Left (NativeCode name arity body)) where
    body = \args -> do
        --traceM $ "arg': " ++ show arg'
        --traceM $ "env: " ++ show (env `M.difference` builtins)
        if length args == arity then (sequence . map evalM $ args) >>= core else yieldFail ImproperCall

-- * builtinConfigs

builtinConfigs :: [(Ident, Either NativeCode Expr)]
builtinConfigs = [

    -- ** core

    macro "let" 3 (\[pat, v, e] -> do
        evalM (App (Lam pat e) v)
        --extraEnv <- matchM pat v
        --withExtraEnvs extraEnv (evalM e)
    ),

    macro "assume" 2 (\[pred, e] -> do
        p <- evalM pred
        case p of
            (Boolean True) -> evalM e
            (Boolean False) -> yieldFail ImproperCall
            _ -> yieldFail (LogicalError "assume predication must be Boolean")
    ),

    macro "assert" 2 (\[pred, e] -> do
        p <- evalM pred
        case p of
            (Boolean True) -> evalM e
            (Boolean False) -> yieldFail (LogicalError "assertion failed")
            _ -> yieldFail (LogicalError "assert predication must be Boolean")
    ),

    -- ** arith

    nativeFn "add" 2 (\case [Number y, Number x] -> yieldSucc (Number (x + y)); _ -> yieldFail ImproperCall),
    nativeFn "sub" 2 (\case [Number y, Number x] -> yieldSucc (Number (x - y)); _ -> yieldFail ImproperCall),
    nativeFn "mul" 2 (\case [Number y, Number x] -> yieldSucc (Number (x * y)); _ -> yieldFail ImproperCall),
    nativeFn "pow" 2 (\case [Number y, Number x] -> yieldSucc (Number (x ** y)); _ -> yieldFail ImproperCall),
    nativeFn "div" 2 (\case [Number y, Number x] -> if y == 0 then yieldFail ImproperCall else yieldSucc (Number (x / y)); _ -> yieldFail ImproperCall),
    nativeFn "mod" 2 (\case [Number y, Number x] -> if y == 0 then yieldFail ImproperCall else yieldSucc (Number (x + y)); _ -> yieldFail ImproperCall),
    nativeFn "exactDiv" 2 (\case [Number y, Number x] -> if y == 0 then yieldFail ImproperCall else yieldSucc (Number (x + y)); _ -> yieldFail ImproperCall),

    -- ** compare

    nativeFn "eq" 2 (\case
        [Null, Null] -> yieldSucc (Boolean True)
        [Boolean y, Boolean x] -> yieldSucc (Boolean (x == y))
        [Number y, Number x] -> yieldSucc (Boolean (x == y))
        [Text y, Text x] -> yieldSucc (Boolean (x == y))
        --[List xs, List ys] -> error ("TODO")
        --[Dict ps, Dict qs] -> error ("TODO")
        _ -> yieldFail ImproperCall),

    term "neq" "x? y? not (eq# [y, x])",

    nativeFn "lt" 2 (\case [Number y, Number x] -> yieldSucc (Boolean (x < y)); _ -> yieldFail ImproperCall),
    nativeFn "le" 2 (\case [Number y, Number x] -> yieldSucc (Boolean (x <= y)); _ -> yieldFail ImproperCall),
    nativeFn "gt" 2 (\case [Number y, Number x] -> yieldSucc (Boolean (x > y)); _ -> yieldFail ImproperCall),
    nativeFn "ge" 2 (\case [Number y, Number x] -> yieldSucc (Boolean (x >= y)); _ -> yieldFail ImproperCall),

    -- ** logic

    nativeFn "and" 2 (\case [Boolean y, Boolean x] -> yieldSucc (Boolean (x && y)); _ -> yieldFail ImproperCall),
    nativeFn "or" 2 (\case [Boolean y, Boolean x] -> yieldSucc (Boolean (x || y)); _ -> yieldFail ImproperCall),
    nativeFn "not" 1 (\case [Boolean x] -> yieldSucc (Boolean (not x)); _ -> yieldFail ImproperCall),

    -- ** type judge

    nativeFn "isBoolean" 1 (\case [Boolean _] -> yieldSucc (Boolean True); _ -> yieldSucc (Boolean False)),
    nativeFn "isNumber" 1 (\case [Number _] -> yieldSucc (Boolean True); _ -> yieldSucc (Boolean False)),
    nativeFn "isText" 1 (\case [Text _] -> yieldSucc (Boolean True); _ -> yieldSucc (Boolean False)),
    nativeFn "isList" 1 (\case [List _] -> yieldSucc (Boolean True); _ -> yieldSucc (Boolean False)),
    nativeFn "isDict" 1 (\case [Dict _] -> yieldSucc (Boolean True); _ -> yieldSucc (Boolean False)),

    -- ** control

    macro "if" 3 (\[pred, x, y] -> do
        p <- evalM pred
        case p of
            (Boolean True) -> evalM x
            (Boolean False) -> evalM y
            _ -> yieldFail (LogicalError "if predication must be Boolean")
    ),
    term "fixpoint" "f? (x? f (y? (x x) y)) (x? f (y? (x x) y))"]

    

-- * natives & builtins

natives :: NativePool
builtins :: Env

(natives, builtins) = iter 0 builtinConfigs [] [] where
    iter i [] natives builtins = (A.array (0,1000) natives, M.fromList builtins)
    iter i ((name,cfg):cfgs) natives builtins = case cfg of
        Left nc@(NativeCode name arity body) -> iter (i+1) cfgs ((i,nc) : natives) ((name, Native arity i []) : builtins)
        Right expr -> iter i cfgs natives ((name, expr) : builtins)

-- * eval'

eval' :: Int -> Expr -> EvalResult Expr
eval' = eval natives builtins

