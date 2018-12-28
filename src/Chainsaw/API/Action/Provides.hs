
-- | Reader-like wrapper.
module Chainsaw.API.Action.Provides where

import Control.Monad.Trans.Reader

import Data.Traversable
import Data.Foldable

import GHC.Generics

import Chainsaw.API.Apply

-- | Carrier of thing to be provided.
data Provides e a = Provides e a
    deriving (Show, Generic)

-- | This instance allows to push thing, carried by `Provides`
--   into `ReaderT` context for the underlying action.
--
--   For instance: if you have some @Author@, you can provide it,
--   and `ask` for it in underlying action's `apply`.
--
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
