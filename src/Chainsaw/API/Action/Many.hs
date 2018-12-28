
-- | Kleene star combination.
module Chainsaw.API.Action.Many where

import Control.Arrow

import Data.Traversable
import Data.Foldable
import Data.Monoid

import Chainsaw.API.Apply

-- | This instance allows to run a sequence of operations of one type.
--
instance
    ( Apply a undo m res
    , Monoid res
    )
  =>
      Apply [a] [undo] m res
  where
    apply boxes = do
        res <- for boxes $ \a -> do
            apply a

        return $ first mconcat $ unzip res

    undo boxes = do
        ress <- for (reverse boxes) $ \a -> do
            undo a

        return $ mconcat ress
