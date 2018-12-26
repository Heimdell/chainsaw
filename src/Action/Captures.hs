
module Action.Captures where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Data.Traversable
import Data.Foldable

import GHC.Generics

import API

newtype Captures e a = Captures a
    deriving (Show, Generic)

class Has e es | es -> e where
    view :: es -> e

instance
    ( Apply a (ReaderT e m) res
    , Monad m
    , Has e es
    )
  =>
      Apply (Captures e a) (ReaderT es m) res
  where
    newtype Undo (Captures e a)
        = UndoCaptures
            { getUndoCaptures :: Captures e (Undo a)
            }

    apply (Captures a) = do
        env <- asks view
        (res, undo) <- lift $ apply a `runReaderT` env
        return (res, UndoCaptures (Captures undo))

    undo (UndoCaptures (Captures a)) = do
        env <- asks view
        lift $ undo a `runReaderT` env

deriving instance (Show (Undo a)) => Show (Undo (Captures e a))
