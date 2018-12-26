
module Action.And where

import API

instance
    ( Apply a undoA m resA
    , Apply b undoB m resB
    )
  =>
      Apply (a, b) (undoA, undoB) m (resA, resB)
  where
    apply (a, b) = do
        (resA, undoA) <- apply a
        (resB, undoB) <- apply b
        return ((resA, resB), (undoA, undoB))

    undo (a, b) = do
        resB <- undo b
        resA <- undo a
        return (resA, resB)
