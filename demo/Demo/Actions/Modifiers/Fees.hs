
module Demo.Actions.Modifiers.Fees where

import Control.Monad.Reader

import Chainsaw.API

import Demo.Entities

data PayFees a = PayFees a
    deriving Show

instance Pretty a => Pretty (PayFees a) where
    pretty (PayFees a) =
        wrap "PayFees" (pretty a)

instance
    ( Apply a undo (ReaderT Author m) Proof
    , Access Address Account m
    , Reading Fees TheFees m
    , MonadReader Env m
    )
  =>
      Apply (PayFees a) (PayFees undo) (ReaderT Author m) Proof
  where
    apply (PayFees a) = do
        (res, undoer) <- apply a

        Author  author _ <- ask
        Miner   miner    <- lift $ asks envMiner
        Account _ source <- retrieve author
        fee              <- getFee $ length (show a)

        check (source > fee) $ Err "can't afford fees"

        p1 <- changeBalance author (-fee)
        p2 <- changeBalance miner  ( fee)

        return (res <> p1 <> p2, PayFees undoer)

    undo (PayFees a) = do
        Author  author _ <- ask
        Miner   miner    <- lift $ asks envMiner
        fee              <- getFee $ length (show a)

        p1 <- changeBalance author ( fee)
        p2 <- changeBalance miner  (-fee)

        res <- undo a

        return (res <> p1 <> p2)
