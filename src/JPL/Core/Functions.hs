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
import Data.Functor.Identity
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

-- ** EvalResult

data FailReason = OutOfFuel | ImproperCall | LogicalError String deriving (Show, Eq, Ord)

type EvalResult a = Either FailReason a

-- ** FuelT

newtype FuelT m a = FuelT { runFuelT :: Int -> Maybe (m a, Int) }

instance (Monad m) => Functor (FuelT m) where
    fmap f m = FuelT $ \ s -> do
        guard (s > 0)
        (m', s') <- runFuelT m (s-1)
        pure (f <$> m', s')
    {-# INLINE fmap #-}

instance (Monad m) => Applicative (FuelT m) where
    pure a = FuelT $ \ s -> do
        guard (s > 0)
        pure (pure a, s-1)
    {-# INLINE pure #-}

    FuelT mf <*> FuelT mx = FuelT $ \ s -> do
        guard (s > 0)
        ~(f, s') <- mf (s-1)
        guard (s' > 0)
        ~(x, s'') <- mx (s'-1)
        return (f <*> x, s'')
    {-# INLINE (<*>) #-}

    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}

instance (Monad m) => Monad (FuelT m) where
    m >>= k = FuelT $ \ s -> do
        guard (s > 0)
        ~(m', s') <- runFuelT m (s-1)
        guard (s' > 0)
        --Just (_hole, s')
        Just ((m' >>= \x -> impossible (runFuelT (k x) s')), s')
    {-# INLINE (>>=) #-}

instance MonadTrans FuelT where
    lift m = FuelT $ \s -> do
        guard (s > 0)
        return (m, s-1)
    {-# INLINE lift #-}

getFuelLeft :: (Monad m) => FuelT m Int
getFuelLeft = FuelT $ \s -> pure (pure s, s)

--newtype FuelT m a = FuelT (MaybeT (StateT Int m) a)
--
---- s -> m (Maybe a, s)
--
---- s -> Maybe (m a, s)
--
--instance (Monad m) => Functor (FuelT m) where
--    fmap f (FuelT m) = FuelT (fmap f m)
--
--instance (Monad m) => Applicative (FuelT m) where
--    pure x = FuelT (pure x)
--    (FuelT mf) <*> (FuelT mx) = FuelT (mf <*> mx)
--
--instance (Monad m) => Monad (FuelT m) where
--    return = pure
--    (FuelT (MaybeT (StateT g))) >>= f = FuelT (MaybeT (StateT h)) where
--        h s = do
--            case (s > 0) of
--                False -> pure (Nothing, 0)
--                True -> do
--                    (ma, s') <- g (s-1)
--                    case (s' > 0) of
--                        False -> pure (Nothing, 0)
--            --when (s' <= 0) _h2
--            case ma of
--                Nothing -> undefined
--                Just a -> case f a of (FuelT (MaybeT (StateT u))) -> u (s'-1)


-- ** Eval

-- | Eval is the monad for eval procedure
newtype Eval a = Eval (ExceptT FailReason (StateT () (FuelT Identity)) a)

instance Functor Eval where
    fmap f (Eval m) = Eval (fmap f m)

instance Applicative Eval where
    pure x = Eval (pure x)
    (Eval mf) <*> (Eval mx) = Eval (mf <*> mx)

instance Monad Eval where
    return = pure
    (Eval m) >>= f = Eval (m >>= \x -> case f x of (Eval y) -> y)

getFuelLevel :: Eval Int
getFuelLevel = Eval (lift (lift getFuelLeft))

type Env = Map Ident (Either NativeFn Expr)

data NativeFn = NativeFn {
    nativeFnName :: String,
    runNativeFn :: (Env -> Expr -> Eval Expr)
}

yield :: EvalResult a -> Eval a
yield r = Eval (except r)

yieldSucc :: a -> Eval a
yieldSucc x = yield (Right x)

yieldFail :: FailReason -> Eval a
yieldFail err = yield (Left err)

runEval :: Eval a -> Int -> (EvalResult a, Int)
runEval (Eval ev) fuel = case runFuelT (runStateT (runExceptT ev) ()) fuel of
    Nothing -> (Left OutOfFuel, 0)
    Just (m, fuelLeft) -> (fst (runIdentity m), fuelLeft)

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
eval fuel env expr = fst (runEval (evalM env expr) fuel)

eval' :: Env -> Expr -> EvalResult Expr
eval' = eval 100000

eval'' :: Expr -> EvalResult Expr
eval'' = eval' M.empty

test = eval'' (App (Lam (Var "x") (Var "x")) (Number 1))

