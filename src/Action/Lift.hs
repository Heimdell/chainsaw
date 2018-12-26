
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
    ( Apply a m res
    , MonadTrans t
    , Monad (t m)
    )
  =>
      Apply (Lift a) (t m) res
  where
    newtype Undo (Lift a)
        = UndoLift
            { getUndoLift :: Lift (Undo a)
            }

    apply (Lift a) = do
        (res, undo) <- lift $ apply a
        return (res, UndoLift (Lift undo))

    undo (UndoLift (Lift a)) = do
        lift $ undo a

deriving instance (Show (Undo a)) => Show (Undo (Lift a))
