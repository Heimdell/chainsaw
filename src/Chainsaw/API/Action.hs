
-- | A pack of combinators over actions to combine
--   simple ones into more complex ones.
--
module Chainsaw.API.Action
    ( Provides (..)
    )
    where

import Chainsaw.API.Action.Either
import Chainsaw.API.Action.And
import Chainsaw.API.Action.Many
import Chainsaw.API.Action.Provides
