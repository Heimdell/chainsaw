
-- | Reader-like wrapper.
module Chainsaw.API.Action.Provides where

import Control.Monad.Trans.Reader

import Data.Traversable
import Data.Foldable

import GHC.Generics

import Chainsaw.API.Apply
import Chainsaw.API.PrettyPrint

-- | Carrier of thing to be provided.
data Provides e a = Provides { providesPayload :: e, providesAction :: a }
    deriving (Show, Generic)

instance (Show e, Pretty a) => Pretty (Provides e a) where
    pretty (Provides e a) = hang (text "Provides") 4 $
        hang (text "{ payload =") 4 (text $ show e) $$
        hang (text ", action =") 4 (pretty a) $$
        text "}"

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
