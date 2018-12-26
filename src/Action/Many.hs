
module Action.Many where

import Control.Arrow

import Data.Traversable
import Data.Foldable

import API

instance
    ( Apply a m res
    )
  =>
      Apply [a] m [res]
  where
    newtype Undo [a]
        = UndoMany
            { getUndoMany :: [Undo a]
            }

    apply boxes = do
        res <- for boxes $ \a -> do
            apply a

        return $ (id *** UndoMany) . unzip $ res

    undo (UndoMany boxes) = do
        for (reverse boxes) $ \a -> do
            undo a

deriving instance (Show (Undo a)) => Show (Undo [a])
