
module Demo.Entities where

import Data.Map (Map)
import qualified Data.Map as Map

import Chainsaw.API

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

data Fees = Fees
    deriving Show

type Proof = Map Address Account

-- | Increment nonce.
touchAccount :: Access Address Account m => Address -> m ()
touchAccount addr = change addr $ \acc -> acc { accountNonce = accountNonce acc + 1 }

-- | Decrement nonce.
unTouchAccount :: Access Address Account m => Address -> m ()
unTouchAccount addr = change addr $ \acc -> acc { accountNonce = accountNonce acc - 1 }

-- | Change account balance (returns excerpt of state).
changeBalance :: Access Address Account m => Address -> Int -> m Proof
changeBalance addr d = do
    old <- retrieve addr
    change addr $ \acc -> acc { accountBalance = accountBalance acc + d }
    return $ Map.singleton addr old

-- | Calculate fee for given tx size.
getFee :: Reading Fees TheFees m => Int -> m Int
getFee txSize = do
    TheFees { k, c } <- retrieve Fees
    return $ c + round (k * fromIntegral txSize)

data Env = Env
    { envMiner :: Miner
    }
    deriving Show

