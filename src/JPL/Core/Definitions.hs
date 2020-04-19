{-# language TupleSections #-}
{-# language FlexibleInstances #-}

{-# language RankNTypes #-}
{-# language UndecidableInstances #-}

{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language StandaloneDeriving, DeriveTraversable #-}

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
import Data.Functor.Foldable.TH ( makeBaseFunctor )

impossible :: a
impossible = undefined

complain :: String -> a --TODO: add stack trace
complain msg = error ("complain: " ++ msg)

--------------------------------------------------------------------------------
-- * ShowLiteral
--------------------------------------------------------------------------------

class ShowLiteral a where
    -- | genLitWith showPart expr = (dispersity, str)
    genLitWith :: (forall a'. ShowLiteral a' => Int -> a' -> String) -> a -> (Int, String)

    showLit :: Int -> a -> String
    showLit dispLimit expr = if disp > dispLimit then "(" ++ s ++ ")" else s where
        (disp, s) = genLitWith showLit expr

--------------------------------------------------------------------------------
-- * Expr
--------------------------------------------------------------------------------

type Ident = String -- ^ info to distinguish different identifiers

data Expr =
      Null
    | Number Double
    | Text String
    | Boolean Bool
    | List [Expr]
    | Dict [(String, Expr)]
    | Var Ident
    | App Expr Expr
    | Lam Pattern Expr
    | Alt Expr Expr -- `f | g` combine two partial function into a new one
    | Let Ident Expr Expr
    | Assume Expr Expr
    | Assert Expr Expr
    | Native Ident -- native 1-ary function, this case is WHNF (which is different from Var)
    deriving (Eq)

type Pattern = Expr -- ^ Pattern is an Expr with only data constructors and vars

-- | check if an Expr is a Weak Head Normal Form
isWHNF :: Expr -> Bool
isWHNF expr = case expr of
    Null -> True
    Number _ -> True
    Text _ -> True
    Boolean _ -> True
    List _ -> True
    Dict _ -> True
    Native _ -> True
    Lam _ _ -> True
    Alt _ _ -> True
    _ -> False

-- | check if an Expr is a valid Pattern
isPattern :: Expr -> Bool
isPattern expr = case expr of
    Null -> True
    Number _ -> True
    Text _ -> True
    Boolean _ -> True
    List xs -> all isPattern xs
    Dict ps -> all isPattern (map snd ps)
    Var _ -> True
    _ -> False

-- ** show instances

instance ShowLiteral Expr where
    genLitWith showPart expr = case expr of
        Null -> (0, "null")
        Number x -> (0, show x) --TODO: specify format
        Text s -> (0, show s) --TODO: escape
        Boolean b -> (0, if b then "true" else "false")
        List xs -> (0, "[" ++ intercalate ", " [showPart 1 x | x <- xs] ++ "]")
        Dict mp -> (0, "{" ++ intercalate ", " [showKey k ++ ": " ++ showPart 1 v | (k, v) <- mp] ++ "}")
        Native fname -> (0, '#':fname)
        Var id -> (0, id)
        App ef ex -> (1, showPart 1 ef ++ " " ++ showPart 0 ex)
        Let k v e -> (2, k ++ " := " ++ showPart 1 v ++ "; " ++ showPart 2 e)
        Assume p e -> (2, "assume " ++ showPart 1 p ++ "; " ++ showPart 2 e)
        Assert p e -> (2, "assert " ++ showPart 1 p ++ "; " ++ showPart 2 e)
        Lam pat e -> (3, showPart 0 pat ++ "? " ++ showPart 3 e)
        Alt ef eg -> (4, showPart 3 ef ++ " | " ++ showPart 4 eg)
        where
            showKey k = if True then k else show k --TODO: escape

instance Show Expr where
    show expr = "Expr `" ++ (showLit maxBound expr) ++ "`"

-------------------------------------------------------------------------------------------
-- * ExprF
-------------------------------------------------------------------------------------------

-- | data ExprF e
makeBaseFunctor ''Expr

-------------------------------------------------------------------------------------------
-- * EvalResult
-------------------------------------------------------------------------------------------

--data EvalResult a =
--      Success a
--    | OutOfFuel
--    | ImproperCall
--    | LogicalError String
--    deriving (Show, Eq)
--
--instance Functor EvalResult where
--    fmap f r = case r of
--        Success x -> Success (f x)
--        OutOfFuel -> OutOfFuel
--        ImproperCall -> ImproperCall
--        LogicalError msg -> LogicalError msg
--
--instance Applicative EvalResult where
--    pure = Success
--    (Success f) <*> (Success x) = Success (f x)
--    mf <*> (Success x) = fmap impossible mf
--    (Success f) <*> mx = fmap impossible mx
--
--instance Monad EvalResult where
--    return = pure
--    (Success x) >>= mf = (mf x)
--    mx >>= mf = fmap impossible mx

