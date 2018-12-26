
module Action.And where

import API

instance
    ( Apply a m resA
    , Apply b m resB
    )
  =>
      Apply (a, b) m (resA, resB)
  where
    newtype Undo (a, b)
        = UndoAnd
            { getUndoAnd :: (Undo a, Undo b)
            }

    apply (a, b) = do
        (resA, undoA) <- apply a
        (resB, undoB) <- apply b
        return ((resA, resB), UndoAnd (undoA, undoB))

    undo (UndoAnd (a, b)) = do
        resB <- undo b
        resA <- undo a
        return (resA, resB)

deriving instance (Show (Undo a), Show (Undo b)) => Show (Undo (a, b))
