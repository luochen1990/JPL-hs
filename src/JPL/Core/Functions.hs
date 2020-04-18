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

matchPattern :: Pattern -> Expr -> Maybe [(Ident, Expr)]
matchPattern pat expr = match pat expr [] where
    match :: Pattern -> Expr -> [(Ident, Expr)] -> Maybe [(Ident, Expr)]
    match pat expr bindings = case (pat, expr) of
        (Null, Null) -> Just bindings
        (Number x, Number x') -> if x == x' then Just bindings else Nothing
        (Text s, Text s') -> if s == s' then Just bindings else Nothing
        (Boolean b, Boolean b') -> if b == b' then Just bindings else Nothing
        (List xs, List xs') -> if length xs == length xs' then concat <$> sequence [match pat v [] | (pat, v) <- zip xs xs'] else Nothing
        (Dict mp, Dict mp') -> concat <$> sequence [if isJust mv then match pat (fromJust mv) [] else Nothing | (k, pat) <- mp, let mv = lookup k mp']
        (Var id, _) -> if id == "_" then Just bindings else Just ((id, expr) : bindings) --NOTE: _ is an special identifier
        _ -> complain "given `pat` is not a valid Pattern"

-- ** FuelT

newtype FuelT m a = FuelT { runFuelT :: Int -> MaybeT m (a, Int) }

instance (Monad m) => Functor (FuelT m) where
    fmap f m = FuelT $ \ s ->
        if s > 0 then fmap (\ ~(a, s') -> (f a, s'-1)) $ runFuelT m s else empty
    {-# INLINE fmap #-}

instance (Monad m) => Applicative (FuelT m) where
    pure a = FuelT $ \ s -> if s > 0 then return (a, s-1) else empty
    {-# INLINE pure #-}

    FuelT mf <*> FuelT mx = FuelT $ \ s -> do
        guard (s > 0)
        ~(f, s') <- mf (s-1)
        guard (s' > 0)
        ~(x, s'') <- mx (s'-1)
        return (f x, s'')
    {-# INLINE (<*>) #-}

    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}

instance (Monad m) => Monad (FuelT m) where
    m >>= k  = FuelT $ \ s -> do
        guard (s > 0)
        ~(a, s') <- runFuelT m (s-1)
        runFuelT (k a) s'
    {-# INLINE (>>=) #-}

instance MonadTrans FuelT where
    lift m = FuelT $ \s -> do
        guard (s > 0)
        a <- lift m
        MaybeT $ return (Just (a, s-1))
    {-# INLINE lift #-}

-- ** EvalM

-- | EvalM is the monad for eval procedure
type EvalM a = FuelT (StateT () EvalResult) a

data Expr' = MkExpr' (ExprF Expr') | NativeFn (Expr -> EvalM Expr)

type Env = Map Ident Expr'

yield :: EvalResult a -> EvalM a
yield r = FuelT (\n -> MaybeT (StateT $ \ ~() -> fmap (\a -> (Just (a, n), ())) r))

runEvalM :: EvalM a -> Int -> EvalResult a
runEvalM proc fuel = case runStateT (runMaybeT (runFuelT proc fuel)) () of
    (Success (mr, s)) -> case mr of Nothing -> OutOfFuel; Just (r, fuelLeft) -> Success r
    err -> fmap impossible err

-- ** evalM

evalM :: Env -> Expr -> EvalM Expr
evalM env expr = do
    r <- evalM' env expr
    case r of
        MkExpr' e -> yield (Success (toExpr r))
        NativeFn fn -> yield (LogicalError "got native fn as final result")

toExpr :: Expr' -> Expr
toExpr = undefined

toExpr' :: Expr -> Expr'
toExpr' = undefined

evalM' :: Env -> Expr -> EvalM Expr'
evalM' env expr = case expr of
    Null -> yield (Success (MkExpr' NullF))
    Number x -> yield (Success (MkExpr' (NumberF x)))
    Text s -> yield (Success (MkExpr' (TextF s)))
    Boolean b -> yield (Success (MkExpr' (BooleanF b)))
    List es -> (MkExpr' . ListF) <$> sequence [(evalM' env e) | e <- es]
    Dict mp -> (MkExpr' . DictF) <$> sequence [(k,) <$> (evalM' env e) | (k, e) <- mp]
    Var id -> case (M.lookup id env) of
        Just e' -> yield (Success e')
        Nothing -> yield (LogicalError "variable not found")
    App ef ex -> do
        f <- evalM' env ef
        case f of
            (MkExpr' (LamF pat e)) -> do
                x <- evalM env ex
                case matchPattern (toExpr pat) x of
                    Nothing -> yield (LogicalError "pattern not match")
                    Just extraEnv -> evalM' (M.union env (M.fromList (map (second toExpr') extraEnv))) (toExpr e)
            --(MkExpr' (AltF eg eh)) -> 
            --(NativeFn fn) -> do
            --    x <- evalM' env ex
            --    case x of
    Lam pat e -> yield (Success (MkExpr' (LamF (toExpr' pat) (toExpr' e))))
    Let k v e -> evalM' (M.insert k (toExpr' v) env) e
    Assume ep e -> do
        p <- evalM' env ep
        case p of
            (MkExpr' (BooleanF True)) -> evalM' env e
            (MkExpr' (BooleanF False)) -> yield $ ImproperCall
            _ -> yield $ LogicalError "assume cond must be Boolean"
    Assert ep e -> do
        p <- evalM' env ep
        case p of
            (MkExpr' (BooleanF True)) -> evalM' env e
            (MkExpr' (BooleanF False)) -> yield $ LogicalError "assertion failed"
            _ -> yield $ LogicalError "assert cond must be Boolean"

-- ** eval

eval :: Int -> Env -> Expr -> EvalResult Expr
eval fuel env expr = runEvalM (evalM env expr) fuel

eval' :: Env -> Expr -> EvalResult Expr
eval' = eval 100000

eval'' :: Expr -> EvalResult Expr
eval'' = eval' M.empty

