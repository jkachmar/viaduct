module App where

import Capabilities.Logger (Logger)
import Capabilities.Logger.Katip
  ( HasKatipConfig (..),
    KatipConfig,
    KatipLogger (..),
    KatipM (..),
  )
import Katip (Katip, KatipContext)
import Katip.Monadic (NoLoggingT (..))
import RIO

--------------------------------------------------------------------------------

-- | Run some 'App' with the provided 'Config'.
runApp :: MonadIO m => Config -> App a -> m a
runApp cfg (App app) = liftIO (app cfg)

-- | The application type.
--
-- This acts as an environment with access to a top-level 'Config' object as
-- well as the production implementation for various "capabilities" defined in
-- terms of some typeclass interface and newtype "carrier".
newtype App a = App
  { unApp :: Config -> IO a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader Config,
      MonadUnliftIO
    )
    via (RIO Config)
  -- deriving
  --   (Katip, KatipContext)
  --   via (NoLoggingT App)
  deriving
    (Katip, KatipContext)
    via (KatipM App)
  deriving
    (Logger)
    via (KatipLogger App)

-- | Application-wide configuration and shared mutable state.
--
-- This object stores the configuration information that our application needs
-- to function, as well as any stateful references which might need to be
-- tracked between individual components within our architecture.
data Config = Config
  { -- | 'Katip' logger configuration.
    katipConfig :: KatipConfig
  }

instance HasKatipConfig Config where
  katipConfigL = lens getter setter
    where
      getter Config {katipConfig} = katipConfig
      setter cfg katipConfig = cfg {katipConfig}
