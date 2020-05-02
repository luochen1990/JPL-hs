{-# language TupleSections #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}

-- | Providing core definitions about JsonSpec

module JPL.Core.Functions where

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Array as A
import Data.Array (Array)
import Control.Arrow
import Control.Monad
import Control.Applicative
-- import qualified Control.Monad.State.Class as State
-- import Control.Monad.State (State, StateT(..), runStateT)
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Functor.Identity
import Data.Functor.Foldable (embed, project, cata, para)
import Data.Semigroup
import Data.Foldable (fold, foldMap, toList)
import Data.List
import Data.Maybe
import Data.Fixed (mod')
import Data.Text.Lazy (unpack)
import Data.Char (isAlphaNum)
import Text.Pretty.Simple
import JPL.Core.Definitions
import Debug.Trace

--------------------------------------------------------------------------------
-- * eval
--------------------------------------------------------------------------------

-- ** EvalResult

data FailReason = OutOfFuel | Complain String | ImproperCall String | LogicalError String deriving (Show, Eq, Ord)

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

data NativeCode = NativeCode {
    nativeCodeName :: String,
    nativeArity :: Int,
    runNative :: ([Expr] -> Eval Expr)
}

instance Show NativeCode where
    show (NativeCode name arity body) = "(* native " ++ show arity ++ "-ary: " ++ name ++ "  *)"

type NativePool = Array Int NativeCode

type Env = Map Ident Expr

-- | Eval is the monad for eval procedure
newtype Eval a = Eval {unpackEval :: ExceptT FailReason (RWST NativePool () Env (FuelT Identity)) a}

instance Functor Eval where
    fmap f (Eval m) = Eval (fmap f m)

instance Applicative Eval where
    pure x = Eval (pure x)
    (Eval mf) <*> (Eval mx) = Eval (mf <*> mx)

instance Monad Eval where
    return = pure
    (Eval m) >>= f = Eval (m >>= \x -> unpackEval (f x))

getNativePool :: Eval NativePool
getNativePool = Eval (lift ask)

getEnv :: Eval Env
getEnv = Eval (lift get)

setEnv :: Env -> Eval ()
setEnv env = Eval (lift (put env))

withExtraEnv :: Ident -> Expr -> Eval a -> Eval a
withExtraEnv k v ev = do
    env <- getEnv
    setEnv (M.insert k v env)
    r <- ev
    setEnv env
    return r

withExtraEnvs :: [(Ident, Expr)] -> Eval a -> Eval a
withExtraEnvs extras ev = do
    env <- getEnv
    setEnv (foldr (uncurry M.insert) env extras)
    r <- ev
    setEnv env
    return r

yield :: EvalResult a -> Eval a
yield r = Eval (except r)

yieldSucc :: a -> Eval a
yieldSucc x = yield (Right x)

yieldFail :: FailReason -> Eval a
yieldFail err = yield (Left err)

runFuel :: FuelT Identity a -> Int -> Maybe (a, Int)
runFuel m fuel = runIdentity (runMaybeT (runFuelT m fuel))

runEval :: Eval a -> NativePool -> Env -> Int -> (EvalResult a, (Int, Maybe (Env, ())))
runEval (Eval ev) nativePool env fuel = case runFuel (runRWST (runExceptT ev) nativePool env) fuel of
    Nothing -> (Left OutOfFuel, (0, Nothing))
    Just ((a,s,w), fuelLeft) -> (a, (fuelLeft, Just (s, w)))

-- ** matchM

matchM :: Pattern -> Expr -> Eval [(Ident, Expr)]
matchM pat expr = matM pat expr where
    matM pat expr = do
        e <- evalM expr
        case pat of
            Null -> case e of
                Null -> (yieldSucc [])
                _ -> yieldFail (ImproperCall ("pattern `" ++ showLit maxBound pat ++ "` not match"))
            Number x -> case e of
                (Number x') -> if x == x' then (yieldSucc []) else yieldFail (ImproperCall ("pattern `" ++ showLit maxBound pat ++ "` not match")) --TODO: precise issue about Double?
                _ -> yieldFail (ImproperCall ("pattern `" ++ showLit maxBound pat ++ "` not match"))
            Text s -> case e of
                (Text s') -> if s == s' then (yieldSucc []) else yieldFail (ImproperCall ("pattern `" ++ showLit maxBound pat ++ "` not match"))
                _ -> yieldFail (ImproperCall ("pattern `" ++ showLit maxBound pat ++ "` not match"))
            Boolean b -> case e of
                (Boolean b') -> if b == b' then (yieldSucc []) else yieldFail (ImproperCall ("pattern `" ++ showLit maxBound pat ++ "` not match"))
                _ -> yieldFail (ImproperCall ("pattern `" ++ showLit maxBound pat ++ "` not match"))
            List xs -> case e of
                (List xs') -> if length xs == length xs' then concat <$> sequence [matM pat v | (pat, v) <- zip xs xs'] else yieldFail (ImproperCall ("pattern `" ++ showLit maxBound pat ++ "` not match"))
                _ -> yieldFail (ImproperCall ("pattern `" ++ showLit maxBound pat ++ "` not match"))
            Dict mp -> case e of
                (Dict mp') -> concat <$> sequence [if isJust mv then matM pat (fromJust mv) else yieldFail (ImproperCall ("pattern `" ++ showLit maxBound pat ++ "` not match")) | (k, pat) <- mp, let mv = lookup k mp']
            Var id -> if id == "_" then (yieldSucc []) else (yieldSucc [(id, expr)]) --NOTE: _ is an special identifier
            _ -> complain ("given `pat` is not a valid Pattern: " ++ show pat)

-- ** evalM

allVars :: Pattern -> [Ident]
allVars = cata $ \case (VarF id) -> [id]; limb -> concatMap toList limb

inject :: Ident -> Expr -> Expr -> Expr
inject k v = para $ \expr -> case expr of
    VarF k' -> if k' == k then v else embed (fmap snd expr)
    LamF (pat, _) (e, injected) -> Lam pat (if k `elem` allVars pat then e else injected)
    _ -> embed (fmap snd expr)

injectAll :: [(Ident, Expr)] -> Expr -> Expr
injectAll kvs = foldr (.) id [inject k v | (k, v) <- kvs]

evalM :: Expr -> Eval Expr
evalM expr | isWHNF expr = (yieldSucc expr)
evalM expr = case expr of
    Var id -> do
        env <- getEnv
        case (M.lookup id env) of
            Just e -> evalM e
            Nothing -> yieldFail (LogicalError ("variable `" ++ id ++ "` not found"))
    App ef ex -> do
        --traceM $ "expr': " ++ show expr ++ "\n"
        f <- evalM ef
        case f of
            (Lam pat e) -> do
                extraEnv <- matchM pat ex
                --let env' = M.union env (M.fromList (map (second Right) extraEnv))
                let e' = injectAll extraEnv e
                --traceM $ "e': " ++ show e'
                Eval $ catchE (unpackEval (evalM e')) $ \err ->
                    case err of
                        Complain msg -> throwE (ImproperCall $ msg)
                        ImproperCall msg -> throwE (LogicalError $ "improper call: " ++ msg)
                        _ -> throwE err
            (Alt eg eh) -> Eval $ catchE (unpackEval (evalM (App eg ex))) $ \err ->
                case err of
                    ImproperCall _ -> unpackEval (evalM (App eh ex))
                    _ -> throwE err
            (Native ary addr args) -> evalM (Native (ary - 1) addr (ex : args))
                --case M.lookup fname env of
                --    Just ee -> case ee of
                --        Left fn -> runNative fn env ex
                --        Right e -> impossible
                --    Nothing -> impossible
            _ -> yieldFail (LogicalError ("not a function: `" ++ (showLit maxBound f) ++ "`"))
    Native ary addr args ->
        if ary == 0 then do
            nativePool <- getNativePool
            runNative (nativePool A.! addr) (reverse args)
        else impossible
    -- Let k v e -> evalM (M.insert k (Right v) env) e
    -- Assume ep e -> do
    --     p <- evalM env ep
    --     case p of
    --         (Boolean True) -> evalM env e
    --         (Boolean False) -> yieldFail ImproperCall
    --         _ -> yieldFail (LogicalError "assume cond must be Boolean")
    -- Assert ep e -> do
    --     p <- evalM env ep
    --     case p of
    --         (Boolean True) -> evalM env e
    --         (Boolean False) -> yieldFail (LogicalError "assertion failed")
    --         _ -> yieldFail (LogicalError "assert cond must be Boolean")

-- ** eval

eval :: NativePool -> Env -> Int -> Expr -> EvalResult Expr
eval pool env fuel expr = fst (runEval (evalM expr) pool env fuel)

