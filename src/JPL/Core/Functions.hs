{-# language TupleSections #-}
{-# language FlexibleInstances #-}

-- | Providing core definitions about JsonSpec

module JPL.Core.Functions where

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Control.Arrow
import Control.Monad
import Control.Applicative
import qualified Control.Monad.State.Class as State
import Control.Monad.State (State, StateT(..), runStateT)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Semigroup
import Data.Foldable (fold, foldMap)
import Data.List
import Data.Maybe
import Data.Fixed (mod')
import Data.Text.Lazy (unpack)
import Data.Char (isAlphaNum)
import Text.Pretty.Simple
import JPL.Core.Definitions

--------------------------------------------------------------------------------
-- * eval
--------------------------------------------------------------------------------

-- ** matchPattern

--matchPattern :: Pattern -> Expr -> Maybe [(Ident, Expr)]
--matchPattern pat expr = match pat expr [] where
--    match :: Pattern -> Expr -> [(Ident, Expr)] -> Maybe [(Ident, Expr)]
--    match pat expr bindings = case (pat, expr) of
--        (Null, Null) -> Just bindings
--        (Number x, Number x') -> if x == x' then Just bindings else Nothing
--        (Text s, Text s') -> if s == s' then Just bindings else Nothing
--        (Boolean b, Boolean b') -> if b == b' then Just bindings else Nothing
--        (List xs, List xs') -> if length xs == length xs' then concat <$> sequence [match pat v [] | (pat, v) <- zip xs xs'] else Nothing
--        (Dict mp, Dict mp') -> concat <$> sequence [if isJust mv then match pat (fromJust mv) [] else Nothing | (k, pat) <- mp, let mv = lookup k mp']
--        (Var id, _) -> if id == "_" then Just bindings else Just ((id, expr) : bindings) --NOTE: _ is an special identifier
--        _ -> complain "given `pat` is not a valid Pattern"

-- ** FuelT

--newtype FuelT m a = FuelT { runFuelT :: Int -> (MaybeT m a, Int) }
--
--instance (Monad m) => Functor (FuelT m) where
--    fmap f m = FuelT $ \ s ->
--        if s > 0 then fmap f &&& (flip (-) 1) $ runFuelT m s else (empty, s)
--    {-# INLINE fmap #-}
--
--instance (Monad m) => Applicative (FuelT m) where
--    pure a = FuelT $ \s -> if s > 0 then (return a, s-1) else (empty, s)
--    {-# INLINE pure #-}
--
--    FuelT mf <*> FuelT mx = FuelT $ \ s -> if s > 0 then let (f, s') = mf (s-1) in do
--        guard (s > 0)
--        ~(f, s') <- mf (s-1)
--        guard (s' > 0)
--        ~(x, s'') <- mx (s'-1)
--        return (f x, s'')
--    {-# INLINE (<*>) #-}
--
--    m *> k = m >>= \_ -> k
--    {-# INLINE (*>) #-}
--
--instance (Monad m) => Monad (FuelT m) where
--    m >>= k  = FuelT $ \ s -> do
--        guard (s > 0)
--        ~(a, s') <- runFuelT m (s-1)
--        runFuelT (k a) s'
--    {-# INLINE (>>=) #-}
--
--instance MonadTrans FuelT where
--    lift m = FuelT $ \s -> do
--        guard (s > 0)
--        a <- lift m
--        MaybeT $ return (Just (a, s-1))
--    {-# INLINE lift #-}

--getFuelLevel :: (Monad m) => FuelT m Int
--getFuelLevel = FuelT $ \s -> pure (s, s)

-- ** FuelT

--newtype FuelT m a = m (MaybeT (StateT Int)) a

-- ** Eval

data FailReason = OutOfFuel | ImproperCall | LogicalError String

type EvalResult a = Either FailReason a

newtype Eval a = Eval (ExceptT FailReason (StateT () (MaybeT (StateT Int IO))) a)

instance Functor Eval where
    fmap f (Eval m) = Eval undefined

instance Applicative Eval where
    pure = undefined
    mf <*> mx = undefined

instance Monad Eval where
    return = pure
    m >>= f = undefined

getFuelLevel :: Eval Int
getFuelLevel = undefined
--getFuelLevel = FuelT $ \s -> pure (s, s)

---- | Eval is the monad for eval procedure
--type Eval a = FuelT (StateT () EvalResult) a

type Env = Map Ident (Either NativeFn Expr)

data NativeFn = NativeFn {
    nativeFnName :: String,
    runNativeFn :: (Env -> Expr -> Eval Expr)
}

--yield :: EvalResult a -> Eval a
--yield r = FuelT (\n -> MaybeT (StateT $ \ ~() -> fmap (\a -> (Just (a, n), ())) r))

yield :: EvalResult a -> Eval a
yield r = undefined

yieldSucc :: a -> Eval a
yieldSucc x = yield (Right x)

yieldFail :: FailReason -> Eval a
yieldFail err = yield (Left err)

runEval :: Eval a -> Int -> (EvalResult a, Int)
runEval = undefined
--runEval proc fuel = case runStateT (runMaybeT (runFuelT proc fuel)) () of
--    Right (mr, s) -> case mr of
--        Nothing -> (OutOfFuel, 0)
--        Just (r, fuelLeft) -> Right (r, fuelLeft)
--    err -> fmap impossible err

-- ** matchM

matchM :: Env -> Pattern -> Expr -> Eval [(Ident, Expr)]
matchM env pat expr = matM pat expr [] where
    matM pat expr bindings = do
        expr' <- evalM env expr
        case (pat, expr') of
            (Null, Null) -> (yieldSucc bindings)
            (Number x, Number x') -> if x == x' then (yieldSucc bindings) else yieldFail (LogicalError "not match")
            (Text s, Text s') -> if s == s' then (yieldSucc bindings) else yieldFail (LogicalError "not match")
            (Boolean b, Boolean b') -> if b == b' then (yieldSucc bindings) else yieldFail (LogicalError "not match")
            (List xs, List xs') -> if length xs == length xs' then concat <$> sequence [matM pat v [] | (pat, v) <- zip xs xs'] else yieldFail (LogicalError "not match")
            (Dict mp, Dict mp') -> concat <$> sequence [if isJust mv then matM pat (fromJust mv) [] else yieldFail (LogicalError "not match") | (k, pat) <- mp, let mv = lookup k mp']
            (Var id, _) -> if id == "_" then (yieldSucc bindings) else (yieldSucc ((id, expr) : bindings)) --NOTE: _ is an special identifier
            _ -> complain "given `pat` is not a valid Pattern"

-- ** evalM

evalM :: Env -> Expr -> Eval Expr
--evalM = undefined
evalM _ expr | isWHNF expr = (yieldSucc expr)
evalM env expr = case expr of
    Let k v e -> evalM (M.insert k (Right v) env) e
    Var id -> case (M.lookup id env) of
        Just ee -> case ee of
            Left fn -> (yieldSucc (Native id))
            Right e -> evalM env e
        Nothing -> yieldFail (LogicalError ("variable `" ++ id ++ "` not found"))
    App ef ex -> do
        f <- evalM env ef
        case f of
            (Lam pat e) -> do
                extraEnv <- matchM env pat ex
                let env' = M.union env (M.fromList (map (second Right) extraEnv))
                evalM env' e
            (Alt eg eh) -> do
                fuel <- getFuelLevel
                let (res1, fuelLeft) = runEval (evalM env (App eg ex)) fuel
                case res1 of
                    Right r -> yieldSucc r
                    Left failReason -> case failReason of
                        ImproperCall -> evalM env (App eh ex)
                        _ -> yieldFail failReason
            (Native fname) -> do
                case M.lookup fname env of
                    Just ee -> case ee of
                        Left fn -> runNativeFn fn env ex
                        Right e -> impossible
                    Nothing -> impossible
            _ -> yieldFail (LogicalError ("not a function: " ++ show f))
    --Assume ep e -> do
    --    p <- evalM env ep
    --    case p of
    --        (Boolean True) -> evalM env e
    --        (Boolean False) -> yield $ ImproperCall
    --        _ -> yield $ Left $ LogicalError "assume cond must be Boolean"
    --Assert ep e -> do
    --    p <- evalM env ep
    --    case p of
    --        (Boolean True) -> evalM env e
    --        (Boolean False) -> yield $ Left $ LogicalError "assertion failed"
    --        _ -> yield $ Left $ LogicalError "assert cond must be Boolean"

-- ** eval

eval :: Int -> Env -> Expr -> EvalResult Expr
eval = undefined
--eval fuel env expr = fmap fst (runEval (evalM env expr) fuel)

eval' :: Env -> Expr -> EvalResult Expr
eval' = eval 100000

eval'' :: Expr -> EvalResult Expr
eval'' = eval' M.empty

