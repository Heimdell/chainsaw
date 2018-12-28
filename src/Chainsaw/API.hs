
-- | This is the API of Chainsaw framework.
--
--   It doesn't do much by itself, but it allows to structure out
--   and separate different aspects of applying transactions:
--   fee payment, nonce-checking, on-chain resource access, etc.
--
module Chainsaw.API
    ( -- * On-chain resource access
      Reading (..)
    , Writing (..)
    , Access
    , retrieve
    , retrieves
    , change

      -- * Errors
    , check
    , Err (..)
    , -- * Application of txs
      Apply (..)
    , Provides (..)
    )
    where

import Chainsaw.API.Apply (Apply (..))
import Chainsaw.API.Access (Reading (..), Writing (..), Access, retrieve, retrieves, change)
import Chainsaw.API.Errors (check, Err (..))
import Chainsaw.API.Action (Provides (..))
