{-# language TupleSections #-}
{-# language FlexibleInstances #-}

-- | Providing core definitions about JsonSpec

module JPL.Core.Definitions where

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Control.Monad
import Control.Applicative
import qualified Control.Monad.State.Class as State
import Control.Monad.State (State, StateT, runStateT)
import Control.Monad.Trans
import Data.Semigroup
import Data.Foldable (fold, foldMap)
import Data.List
import Data.Maybe
import Data.Fixed (mod')
import Data.Text.Lazy (unpack)
import Data.Char (isAlphaNum)
import Text.Pretty.Simple

impossible :: a
impossible = undefined

---------------------------------------------------------------------
-- * Expr
---------------------------------------------------------------------

type Ident = String

data Expr =
      Null
    | Number Double
    | Text String
    | Boolean Bool
    | Dict (Map String Expr)
    | List [Expr]
    -- | Lookup Expr Ident
    -- | Index Expr Expr
    | Var Ident
    | App Expr Expr
    | Lam Ident Expr
    | If Expr Expr Expr
    | Case Expr [(Pattern, Expr)]
    | Let Ident Expr Expr
    | Assume Expr Expr
    | Assert Expr Expr
    deriving (Show, Eq)

data Pattern =
      ConstNullPat
    | ConstNumberPat Double
    | ConstTextPat String
    | ConstBooleanPat Bool
    | DictPat (Map String Pattern)
    | ListPat [Pattern]
    | VarPat Ident
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- * EvalResult
--------------------------------------------------------------------------------

data EvalResult a =
      Success a
    | OutOfFuel
    | ImproperCall
    | LogicalError String
    deriving (Show, Eq)

instance Functor EvalResult where
    fmap f r = case r of
        Success x -> Success (f x)
        OutOfFuel -> OutOfFuel
        ImproperCall -> ImproperCall
        LogicalError msg -> LogicalError msg

instance Applicative EvalResult where
    pure = Success
    (Success f) <*> (Success x) = Success (f x)
    mf <*> (Success x) = fmap impossible mf
    (Success f) <*> mx = fmap impossible mx

instance Monad EvalResult where
    return = pure
    (Success x) >>= mf = (mf x)
    mx >>= mf = fmap impossible mx

