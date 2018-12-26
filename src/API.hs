
module API where

import Control.Exception
import Control.Monad.Catch

import Data.Typeable

class (Monad m, Show action, Show (Undo action)) => Apply action m result | action -> result where
    data Undo action :: *

    apply :: action       -> m (result, Undo action)
    undo  :: Undo action  -> m  result

class (MonadThrow m, Show k, Typeable k) => Access k v m | k -> v where
    tryGet :: k -> m (Maybe v)
    store  :: k -> v -> m ()

data NotFound k = NotFound k
    deriving (Show)

instance (Show k, Typeable k) => Exception (NotFound k)

retrieve :: Access k v m => k -> m v
retrieve k = maybe (throwM $ NotFound k) pure =<< tryGet k

change :: Access k v m => k -> (v -> v) -> m ()
change k f = do
    store k . f =<< retrieve k

retrieves :: Access k v m => k -> (v -> a) -> m a
retrieves k view = view <$> retrieve k
