
module Demo where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
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

data    Author = Author { theAuthor :: Address, authorNonce :: Nonce } deriving Show
newtype Miner  = Miner  { getMiner  :: Address } deriving Show

data Account = Account
    { accountNonce   :: Nonce
    , accountBalance :: Integer
    }
    deriving Show

type Nonce = Integer

---- Helper Types -------------------------------------------------------------

data Env = Env
    { envMiner :: Miner
    }
    deriving Show

data Status = Status
    { statusAccounts :: Map Address Account
    }
    deriving Show

withStatusAccounts :: (Map Address Account -> Map Address Account) -> Status -> Status
withStatusAccounts f (Status a) = Status (f a)

type M = ReaderT Env (StateT Status IO)

type Proof = Map Address Account

---- Utils --------------------------------------------------------------------

check :: (Exception e, MonadThrow m) => Bool -> e -> m ()
check True _ = return ()
check _    e = throwM e

data Err = Err String
    deriving Show

instance Exception Err

---- On-chain access ----------------------------------------------------------

-- This instance will autolift the access
instance
    ( Access k v m
    , MonadThrow m
    , MonadThrow (t m)
    , MonadTrans t
    , Show k
    , Typeable k
    )
  =>
      Access k v (t m)
  where
    tryGet k   = lift $ tryGet k
    store  k v = lift $ store  k v

instance {-# OVERLAPPING #-} Access Address Account M where
    tryGet address = do
        Map.lookup address <$> lift (gets statusAccounts)

    store address account = do
        lift $ modify $ withStatusAccounts $ Map.insert address account

-- Actions over accounts
touchAccount   addr   = change addr $ \acc -> acc { accountNonce   = accountNonce   acc + 1 }
unTouchAccount addr   = change addr $ \acc -> acc { accountNonce   = accountNonce   acc - 1 }
changeBalance  addr d = do
    acc <- retrieve addr
    change addr $ \acc -> acc { accountBalance = accountBalance acc + d }
    return $ Map.singleton addr acc

---- Domain-related Wrappers --------------------------------------------------

newtype CheckNonce a = CheckNonce a deriving Show

instance
    ( Apply a (ReaderT Author m) r
    , Access Address Account m
    , MonadCatch m
    )
  =>
      Apply (CheckNonce a) (ReaderT Author m) r
  where
    data Undo (CheckNonce a) = UndoCheckNonce (Undo a)

    apply (CheckNonce a) = do
        Author author nonce <- ask
        nonce'              <- retrieves author accountNonce

        check (nonce == nonce') $ Err "nonce mismatch"

        touchAccount author

        (res, undo) <- apply a

        return (res, UndoCheckNonce undo)

    undo (UndoCheckNonce a) = do
        res <- undo a

        Author author nonce <- ask
        nonce'              <- retrieves author accountNonce

        unTouchAccount author

        check (nonce == nonce') $ Err "nonce mismatch in undo"

        return res


deriving instance Show (Undo a) => Show (Undo (CheckNonce a))

---- Transaction --------------------------------------------------------------

data Pay = Pay { payWhom :: Address, payHowMuch :: Integer }
    deriving Show

type PayM = ReaderT Author M

instance Apply Pay PayM Proof where
    data Undo Pay = UnPay Pay
        deriving Show

    apply action@ (Pay whom howMuch) = do
        Author author _ <- ask

        src@ (Account _ source) <- retrieve author
        dst                     <- retrieve whom

        check (source >= howMuch) $ Err "author dont have enough money"
        check (author /= whom)    $ Err "can't pay to self"
        check (howMuch > 0)       $ Err "can't pay negative amount"

        p1 <- changeBalance author (-howMuch)
        p2 <- changeBalance whom   ( howMuch)

        return (p1 <> p2, UnPay action)

    undo (UnPay (Pay whom howMuch)) = do
        Author author _ <- ask

        src@ (Account _ source) <- retrieve author
        dst                     <- retrieve whom

        check (author /= whom) $ Err "can't unpay for to self"
        check (howMuch > 0)    $ Err "ca't unpay negative amount"

        p1 <- changeBalance author ( howMuch)
        p2 <- changeBalance whom   (-howMuch)

        return (p1 <> p2)

---- Fee payment --------------------------------------------------------------

data PayFees = PayFees
    deriving Show

instance Apply PayFees PayM Proof where
    data Undo PayFees = UnPayFees PayFees
        deriving Show

    apply PayFees = do
        Author author _         <- ask
        Miner miner             <- lift $ asks envMiner

        src@ (Account _ source) <- retrieve author
        dst                     <- retrieve miner

        check (source > 5) $ Err "can't afford fees"

        p1 <- changeBalance author (-5)
        p2 <- changeBalance miner  ( 5)

        return (p1 <> p2, UnPayFees PayFees)

    undo (UnPayFees PayFees) = do
        Author author _         <- ask
        Miner miner             <- lift $ asks envMiner

        src@ (Account _ source) <- retrieve author
        dst                     <- retrieve miner

        p1 <- changeBalance author ( 5)
        p2 <- changeBalance miner  (-5)

        return (p1 <> p2)

---- Testing Area -------------------------------------------------------------

run :: IO Status
run =
    test $ apply $ Provides (Author 2 0)
        ( CheckNonce
            [ Pay 3 10
            , Pay 1 55
            ]
        , PayFees
        )

test action = do
    action
        `runReaderT` Env (Miner 1)
        `execStateT` Status
            { statusAccounts = Map.fromList
                [ (1, Account 0 100)
                , (2, Account 0 120)
                , (3, Account 0 50)
                ]
            }
