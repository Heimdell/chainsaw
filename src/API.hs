
module API where

import Control.Exception
import Control.Monad.Catch

import Data.Typeable

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
    apply :: action -> m (result, undo)
    undo  :: undo   -> m  result

class (MonadThrow m, Show k, Typeable k) => Reading k v m | k -> v where
    tryGet :: k -> m (Maybe v)

class (MonadThrow m, Show k, Typeable k) => Writing k v m where
    store  :: k -> v -> m ()

type Access k v m = (Reading k v m, Writing k v m)

data NotFound k = NotFound k
    deriving (Show)

instance (Show k, Typeable k) => Exception (NotFound k)

retrieve :: Reading k v m => k -> m v
retrieve k = maybe (throwM $ NotFound k) pure =<< tryGet k

change :: Access k v m => k -> (v -> v) -> m ()
change k f = do
    store k . f =<< retrieve k

retrieves :: Reading k v m => k -> (v -> a) -> m a
retrieves k view = view <$> retrieve k

check :: (Exception e, MonadThrow m) => Bool -> e -> m ()
check True _ = return ()
check _    e = throwM e

data Err = Err String
    deriving Show

instance Exception Err

