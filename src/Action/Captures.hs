
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
    ( Apply a undo (ReaderT e m) res
    , Monad m
    , Has e es
    )
  =>
      Apply (Captures e a) (Captures e undo) (ReaderT es m) res
  where
    apply (Captures a) = do
        env <- asks view
        (res, undo) <- lift $ apply a `runReaderT` env
        return (res, (Captures undo))

    undo (Captures a) = do
        env <- asks view
        lift $ undo a `runReaderT` env
