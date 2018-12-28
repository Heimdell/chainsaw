
module Demo where

import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.Trans.State
import Control.Monad.Identity
import Control.Monad.Catch

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Typeable

import API
import Action

---- Domain Types -------------------------------------------------------------

type Address = Int

data Author = Author
    { theAuthor   :: Address
    , authorNonce :: Nonce
    }
    deriving Show

newtype Miner = Miner
    { getMiner :: Address
    }
    deriving Show

data Account = Account
    { accountNonce   :: Nonce
    , accountBalance :: Int
    }
    deriving Show

type Nonce = Integer

data TheFees = TheFees
    { k :: Float
    , c :: Int
    }
    deriving Show

---- Helper Types -------------------------------------------------------------

data Env = Env
    { envMiner :: Miner
    }
    deriving Show

data Status = Status
    { statusAccounts :: Map Address Account
    , paymentFees    :: TheFees
    }
    deriving Show

data Fees = Fees
    deriving Show

withStatusAccounts :: (Map Address Account -> Map Address Account) -> Status -> Status
withStatusAccounts f (Status a pf) = Status (f a) pf

type M = ReaderT Env (StateT Status IO)

type Proof = Map Address Account

---- On-chain access ----------------------------------------------------------

instance {-# OVERLAPPING #-}
    Reading Address Account M
  where
    tryGet address = do
        Map.lookup address <$> lift (gets statusAccounts)

instance {-# OVERLAPPING #-}
    Writing Address Account M
  where
    store address account = do
        lift $ modify $ withStatusAccounts $ Map.insert address account

instance {-# OVERLAPPING #-}
    Reading Fees TheFees M
  where
    tryGet Fees = do
        Just <$> lift (gets paymentFees)

-- Actions over accounts
touchAccount   addr = change addr $ \acc -> acc { accountNonce = accountNonce acc + 1 }
unTouchAccount addr = change addr $ \acc -> acc { accountNonce = accountNonce acc - 1 }

changeBalance addr d = do
    acc <- retrieve addr
    change addr $ \acc -> acc { accountBalance = accountBalance acc + d }
    return $ Map.singleton addr acc

getFee :: Reading Fees TheFees m => Int -> m Int
getFee txSize = do
    TheFees { k, c } <- retrieve Fees
    return $ c + round (k * fromIntegral txSize)

---- Transaction --------------------------------------------------------------

data Pay = Pay
    { payWhom    :: Address
    , payHowMuch :: Int
    }
    deriving Show

type PayM = ReaderT Author M

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
        Account _ source <- retrieve author

        check (author /= whom) $ Err "can't unpay for to self"
        check (howMuch > 0)    $ Err "can't unpay negative amount"

        p1 <- changeBalance author ( howMuch)
        p2 <- changeBalance whom   (-howMuch)

        return (p1 <> p2)

---- Domain-related Wrappers --------------------------------------------------

newtype CheckNonce a = CheckNonce a
    deriving Show

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

        (res, undo) <- apply a

        return (res, CheckNonce undo)

    undo (CheckNonce a) = do
        res <- undo a

        Author author nonce <- ask
        nonce'              <- retrieves author accountNonce

        unTouchAccount author

        check (nonce == nonce' - 1) $ Err "nonce mismatch in undo"

        return res

---- Fee payment --------------------------------------------------------------

data PayFees a = PayFees a
    deriving Show

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
        (res, undo) <- apply a

        Author  author _ <- ask
        Miner   miner    <- lift $ asks envMiner
        Account _ source <- retrieve author
        fee              <- getFee $ length (show a)

        check (source > fee) $ Err "can't afford fees"

        p1 <- changeBalance author (-fee)
        p2 <- changeBalance miner  ( fee)

        return (p1 <> p2, PayFees undo)

    undo (PayFees a) = do
        Author  author _ <- ask
        Miner   miner    <- lift $ asks envMiner
        Account _ source <- retrieve author
        fee              <- getFee $ length (show a)

        p1 <- changeBalance author ( fee)
        p2 <- changeBalance miner  (-fee)

        res <- undo a

        return (p1 <> p2)

---- Testing Area -------------------------------------------------------------

run :: IO Status
run =
    test $ do
        (res, undoer) <- apply $ Provides (Author 2 0) $ PayFees $
            CheckNonce
                [ Pay 3 10
                , Pay 1 55
                ]
        return ()

test action = do
    action
        `runReaderT` Env (Miner 1)
        `execStateT` Status
            { statusAccounts = Map.fromList
                [ (1, Account 0 100)
                , (2, Account 0 220)
                , (3, Account 0 50)
                ]
            , paymentFees = TheFees 1 10
            }
