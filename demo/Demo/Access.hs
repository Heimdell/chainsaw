
module Demo.Access where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map

import Chainsaw.API

import Demo.Entities
import Demo.Monad

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
