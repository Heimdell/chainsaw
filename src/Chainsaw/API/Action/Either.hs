
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
        (res, undo) <- apply l
        return (res, Left undo)

    apply (Right r) = do
        (res, undo) <- apply r
        return (res, Right undo)

    undo (Left  l) = undo l
    undo (Right r) = undo r
