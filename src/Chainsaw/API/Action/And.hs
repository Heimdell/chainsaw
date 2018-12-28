
-- | Product combination.
--
module Chainsaw.API.Action.And where

import Chainsaw.API.Apply

-- | This instance allows to apply two actions sequentally and
--   undo them in opposite order.
--
instance
    ( Apply a undoA m res
    , Apply b undoB m res
    , Monoid res
    )
  =>
    Apply (a, b) (undoA, undoB) m res
  where
    apply (a, b) = do
        (resA, undoA) <- apply a
        (resB, undoB) <- apply b
        return (resA <> resB, (undoA, undoB))

    undo (a, b) = do
        resB <- undo b
        resA <- undo a
        return (resA <> resB)
