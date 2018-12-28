
-- | Error subsystem.
--
module Chainsaw.API.Errors where

import Control.Exception
import Control.Monad
import Control.Monad.Catch

-- | If conditions fails, throw exception.
check :: (Exception e, MonadThrow m) => Bool -> e -> m ()
check flag e =
    unless flag $ do
        throwM e

-- | "Default" error type.
--
--   You can use it to sketch your `API.Apply.apply` method.
--
data Err = Err String
    deriving Show

-- | Haddock, cool down, its just a mere `Exception` instance!
instance Exception Err
