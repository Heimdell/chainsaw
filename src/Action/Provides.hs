
module Action.Provides where

import Control.Monad.Trans.Reader

import Data.Traversable
import Data.Foldable

import GHC.Generics

import API

data Provides e a = Provides e a
    deriving (Show, Generic)

instance
    ( Apply a (ReaderT e m) res
    , Monad m
    , Show e
    )
  =>
      Apply (Provides e a) m res
  where
    newtype Undo (Provides e a)
        = UndoProvides
            { getUndoProvides :: Provides e (Undo a)
            }

    apply (Provides env a) = do
        (res, undo) <- apply a `runReaderT` env
        return (res, UndoProvides (Provides env undo))

    undo (UndoProvides (Provides env a)) = do
        undo a `runReaderT` env

deriving instance (Show (Undo a), Show e) => Show (Undo (Provides e a))
