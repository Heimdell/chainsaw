
-- | Transaction application
--
module Chainsaw.API.Apply where

-- | Main interface to apply transactions, blocks, etc.
--
--   Hides actions, required to apply the thing.
--
--   Also, @m@ /should/ provide sensible `MonadCatch` instance
--   which does rollback the chain state correctly
--   - if you ever want to rollback.
--
class
    ( Monad m
    , Show action
    , Show undo
    )
  =>
    Apply action undo m result
        | action -> result undo
        , undo   -> action
  where
    -- | Run the action, returning an result and undoer.
    apply :: action -> m (result, undo)

    -- | Undo the action.
    undo :: undo -> m result
