
module Demo.Actions.Pay where

import Control.Monad.Reader

import Chainsaw.API

import Demo.Entities

data Pay = Pay
    { payWhom    :: Address
    , payHowMuch :: Int
    }
    deriving Show

instance Pretty Pay where
    pretty (Pay whom howMuch) =
        block "Pay"
            [ "whom"    =: int whom
            , "howMuch" =: int howMuch
            ]

instance
    Access Address Account m
  =>
    Apply Pay Pay (ReaderT Author m) Proof
  where
    apply action@ (Pay whom howMuch) = do
        Author  author _ <- ask
        Account _ source <- retrieve author

        check (source >= howMuch) $ Err "author dont have enough money"
        check (author /= whom)    $ Err "can't pay to self"
        check (howMuch > 0)       $ Err "can't pay negative amount"

        p1 <- changeBalance author (-howMuch)
        p2 <- changeBalance whom   ( howMuch)

        return (p1 <> p2, action)

    undo (Pay whom howMuch) = do
        Author  author _ <- ask

        check (author /= whom) $ Err "can't unpay for to self"
        check (howMuch > 0)    $ Err "can't unpay negative amount"

        p1 <- changeBalance author ( howMuch)
        p2 <- changeBalance whom   (-howMuch)

        return (p1 <> p2)

