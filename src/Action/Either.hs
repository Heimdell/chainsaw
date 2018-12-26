
module Action.Either where

import API

instance
    ( Apply a m res
    , Apply b m res
    )
  =>
      Apply (Either a b) m res
  where
    newtype Undo (Either a b)
        = UndoEither
            { getUndoEither :: Either (Undo a) (Undo b)
            }

    apply (Left l) = do
        (res, undo) <- apply l
        return (res, UndoEither (Left undo))

    apply (Right r) = do
        (res, undo) <- apply r
        return (res, UndoEither (Right undo))

    undo (UndoEither (Left  l)) = undo l
    undo (UndoEither (Right r)) = undo r

deriving instance (Show (Undo a), Show (Undo b)) => Show (Undo (Either a b))
