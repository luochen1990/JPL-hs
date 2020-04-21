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

getFuelLeft :: (Monad m) => FuelT m Int
getFuelLeft = FuelT $ \s -> pure (s, s)

-- ** Eval

-- | Eval is the monad for eval procedure
newtype Eval a = Eval {unpackEval :: ExceptT FailReason (StateT () (FuelT Identity)) a}

instance Functor Eval where
    fmap f (Eval m) = Eval (fmap f m)

instance Applicative Eval where
    pure x = Eval (pure x)
    (Eval mf) <*> (Eval mx) = Eval (mf <*> mx)

instance Monad Eval where
    return = pure
    (Eval m) >>= f = Eval (m >>= \x -> unpackEval (f x))

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

runFuel :: FuelT Identity a -> Int -> Maybe (a, Int)
runFuel m fuel = runIdentity (runMaybeT (runFuelT m fuel))

runEval :: Eval a -> Int -> (EvalResult a, Int)
runEval (Eval ev) fuel = case runFuel (runStateT (runExceptT ev) ()) fuel of
    Nothing -> (Left OutOfFuel, 0)
    Just ((a, s), fuelLeft) -> (a, fuelLeft)

-- ** matchM

matchM :: Pattern -> Env -> Expr -> Eval [(Ident, Expr)]
matchM pat env expr = matM pat expr where
    matM pat expr = do
        e <- evalM env expr
        case pat of
            Null -> case e of
                Null -> (yieldSucc [])
                _ -> yieldFail ImproperCall
            Number x -> case e of
                (Number x') -> if x == x' then (yieldSucc []) else yieldFail ImproperCall --TODO: precise issue about Double?
                _ -> yieldFail ImproperCall
            Text s -> case e of
                (Text s') -> if s == s' then (yieldSucc []) else yieldFail ImproperCall
                _ -> yieldFail ImproperCall
            Boolean b -> case e of
                (Boolean b') -> if b == b' then (yieldSucc []) else yieldFail ImproperCall
                _ -> yieldFail ImproperCall
            List xs -> case e of
                (List xs') -> if length xs == length xs' then concat <$> sequence [matM pat v | (pat, v) <- zip xs xs'] else yieldFail ImproperCall
                _ -> yieldFail ImproperCall
            Dict mp -> case e of
                (Dict mp') -> concat <$> sequence [if isJust mv then matM pat (fromJust mv) else yieldFail ImproperCall | (k, pat) <- mp, let mv = lookup k mp']
            Var id -> if id == "_" then (yieldSucc []) else (yieldSucc [(id, expr)]) --NOTE: _ is an special identifier
            _ -> complain ("given `pat` is not a valid Pattern: " ++ show pat)

-- ** evalM

evalM :: Env -> Expr -> Eval Expr
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
                extraEnv <- matchM pat env ex
                let env' = M.union env (M.fromList (map (second Right) extraEnv))
                Eval $ catchE (unpackEval (evalM env' e)) $ \err ->
                    case err of
                        ImproperCall -> throwE (LogicalError "improper call")
                        _ -> throwE err
            (Alt eg eh) -> Eval $ catchE (unpackEval (evalM env (App eg ex))) $ \err ->
                case err of
                    ImproperCall -> unpackEval (evalM env (App eh ex))
                    _ -> throwE err
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

evalTest :: Expr -> (EvalResult Expr, Int)
evalTest expr = runEval (evalM M.empty expr) 100

