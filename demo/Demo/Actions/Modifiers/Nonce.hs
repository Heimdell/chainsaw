
module Demo.Actions.Modifiers.Nonce where

import Control.Monad.Reader
import Control.Monad.Catch

import Chainsaw.API

import Demo.Entities

newtype CheckNonce a = CheckNonce a
    deriving Show

instance Pretty a => Pretty (CheckNonce a) where
    pretty (CheckNonce a) =
        wrap "CheckNonce" (pretty a)

instance
    ( Apply a undo (ReaderT Author m) r
    , Access Address Account m
    , MonadCatch m
    )
  =>
      Apply (CheckNonce a) (CheckNonce undo) (ReaderT Author m) r
  where
    apply (CheckNonce a) = do
        Author author nonce <- ask
        nonce'              <- retrieves author accountNonce

        check (nonce == nonce') $ Err "nonce mismatch"
        touchAccount author

        (res, undoer) <- apply a

        return (res, CheckNonce undoer)

    undo (CheckNonce a) = do
        res <- undo a

        Author author nonce <- ask
        nonce'              <- retrieves author accountNonce

        check (nonce == nonce' - 1) $ Err "nonce mismatch in undo"
        unTouchAccount author

        return res
