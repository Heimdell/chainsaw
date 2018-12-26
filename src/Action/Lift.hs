
module Action.Lift where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Data.Traversable
import Data.Foldable

import GHC.Generics

import API

newtype Lift a = Lift a
    deriving (Show, Generic)

instance
    ( Apply a undo m res
    , MonadTrans t
    , Monad (t m)
    )
  =>
      Apply (Lift a) (Lift undo) (t m) res
  where
    apply (Lift a) = do
        (res, undo) <- lift $ apply a
        return (res, Lift undo)

    undo (Lift a) = do
        lift $ undo a
