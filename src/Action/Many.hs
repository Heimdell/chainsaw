
module Action.Many where

import Control.Arrow

import Data.Traversable
import Data.Foldable

import API

instance
    ( Apply a undo m res
    )
  =>
      Apply [a] [undo] m [res]
  where
    apply boxes = do
        res <- for boxes $ \a -> do
            apply a

        return $ unzip res

    undo boxes = do
        for (reverse boxes) $ \a -> do
            undo a
