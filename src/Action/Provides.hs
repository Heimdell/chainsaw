
module Action.Provides where

import Control.Monad.Trans.Reader

import Data.Traversable
import Data.Foldable

import GHC.Generics

import API

data Provides e a = Provides e a
    deriving (Show, Generic)

instance
    ( Apply a undo (ReaderT e m) res
    , Monad m
    , Show e
    )
  =>
      Apply (Provides e a) (Provides e undo) m res
  where
    apply (Provides env a) = do
        (res, undo) <- apply a `runReaderT` env
        return (res, Provides env undo)

    undo (Provides env a) = do
        undo a `runReaderT` env
