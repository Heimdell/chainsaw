
module Demo.Monad where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Chainsaw.API

import Demo.Entities

data Status = Status
    { statusAccounts :: Map Address Account
    , paymentFees    :: TheFees
    }
    deriving Show

instance Pretty Status where
    pretty (Status accs (TheFees { k, c })) =
        block "Status"
            [ "statusAccounts" =: prettyAccs
            , "paymentFees"    =: prettyFees
            ]
      where
        prettyAccs = hang (text "Map.fromList") 4 (list $ map (text . show) $ Map.toList accs)

        prettyFees =
            block "TheFees"
                [ "k" =: float k
                , "c" =: int c
                ]

withStatusAccounts :: (Map Address Account -> Map Address Account) -> Status -> Status
withStatusAccounts f (Status a pf) = Status (f a) pf

type M = ReaderT Env (StateT Status IO)
