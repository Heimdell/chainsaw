
-- | Sum combination.
module Chainsaw.API.Action.Either where

import Chainsaw.API.Apply

-- | This instance allows to apply either of two actions.
--
instance
    ( Apply a undoA m res
    , Apply b undoB m res
    )
  =>
      Apply (Either a b) (Either undoA undoB) m res
  where
    apply (Left l) = do
        (res, undoer) <- apply l
        return (res, Left undoer)

    apply (Right r) = do
        (res, undoer) <- apply r
        return (res, Right undoer)

    undo (Left  l) = undo l
    undo (Right r) = undo r
