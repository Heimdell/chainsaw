
-- | On-chain resource access.
--
module Chainsaw.API.Access where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Trans.Class

import Data.Typeable

-- | Allows to read on-chain resource.
class
    ( MonadThrow m
    , Show k
    , Typeable k
    )
  =>
    Reading k v m
        | k -> v
  where
    -- | Attempt reading.
    tryGet :: k -> m (Maybe v)

-- | Allows to write on-chain resource.
class
    ( Show k
    , Typeable k
    )
  =>
    Writing k v m
        | k -> v
  where
    -- | Perform writing.
    store :: k -> v -> m ()

-- | Combined read-write capability.
type Access k v m = (Reading k v m, Writing k v m)

-- | This instance will `lift` the read access over any `MonadTrans` automatically.
--
--   Caveat emptor: you have to declare your access instances over
--   monad transformer stack synonyms with @OVERLAPPING@ pragma.
--
instance
    ( Reading k v m
    , MonadThrow m
    , MonadThrow (t m)
    , MonadTrans t
    , Show k
    , Typeable k
    )
  =>
    Reading k v (t m)
  where
    tryGet k = lift $ tryGet k

-- | This instance will `lift` the write access over any `MonadTrans` automatically.
--
--   Caveat emptor: you have to declare your access instances over
--   monad transformer stack synonyms with @OVERLAPPING@ pragma.
--
instance
    ( Writing k v m
    , MonadThrow m
    , MonadThrow (t m)
    , MonadTrans t
    , Show k
    , Typeable k
    )
  =>
    Writing k v (t m)
  where
    store k v = lift $ store k v

-- | Exception to be thrown if key is absent.
data NotFound k = NotFound k
    deriving (Show)

-- | Haddock, can you stop?
instance (Show k, Typeable k) => Exception (NotFound k)

-- | Read or throw.
retrieve :: Reading k v m => k -> m v
retrieve k =
    tryGet k
        >>= maybe (throwM $ NotFound k) pure

-- | Apply a function to value behind the key or throw if key is absent.
change :: Access k v m => k -> (v -> v) -> m ()
change k f =
    retrieve k
        >>= store k . f

-- | Read the value, apply a function, return the result.
retrieves :: Reading k v m => k -> (v -> a) -> m a
retrieves k view =
    view <$> retrieve k

