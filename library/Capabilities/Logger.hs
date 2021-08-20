module Capabilities.Logger
  ( -- * Logging capability

    -- ** Typeclass interface
    Logger (..),
  )
where

import Data.Text.Lazy.Builder qualified as TLB (Builder)
import RIO

--------------------------------------------------------------------------------

-- | An abstract interface for logging functions which are capable of reporting
-- source locations.
class Monad m => Logger m where
  debug :: HasCallStack => TLB.Builder -> m ()
  info :: HasCallStack => TLB.Builder -> m ()
  warn :: HasCallStack => TLB.Builder -> m ()
  error :: HasCallStack => TLB.Builder -> m ()
