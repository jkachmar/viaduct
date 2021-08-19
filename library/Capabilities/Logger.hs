{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Capabilities.Logger
  ( -- * Logging capability

    -- ** Typeclass interface
    Logger (..),

    -- *** Katip-based instance carrier
    KatipLogger (..),

    -- ** Configuration
    KatipConfig (..),
    HasKatipConfig (..),
    contextsL,
    logEnvL,
    namespaceL,
  )
where

import Data.Kind (Type)
import Katip
  ( Katip (..),
    KatipContext (..),
    LogContexts,
    LogEnv,
    Namespace,
    Severity (..),
    logFM,
    ls,
  )
import RIO

--------------------------------------------------------------------------------

-- | Typeclass representing an interface for logging `Scribe`s.
class Monad m => Logger m where
  info :: Text -> m ()
  error :: Text -> m ()
  trace :: Text -> m ()

-- | All 'KatipLogger's fulfill the logging interface described by 'Logger'.
instance (MonadIO m, WithKatipConfig env m) => Logger (KatipLogger m) where
  info = logFM InfoS . ls
  error = logFM ErrorS . ls
  trace = logFM DebugS . ls

--------------------------------------------------------------------------------

-- | An newtype whose primary purpose is to serve as a "carrier" for 'Logger'
-- instances powered by 'Katip' and 'IO'.
--
-- This makes it possible to trivially derive all the capabilities provided by
-- 'KatipLogger' for other types with a similar structure.
type KatipLogger :: (Type -> Type) -> Type -> Type
newtype KatipLogger m result = KatipLogger (m result)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader env)

instance (MonadIO m, WithKatipConfig env m) => Katip (KatipLogger m) where
  getLogEnv = view (katipConfigL . logEnvL)
  localLogEnv modifyLogEnv (KatipLogger action) =
    KatipLogger $
      local (over (katipConfigL . logEnvL) modifyLogEnv) action

instance (MonadIO m, WithKatipConfig env m) => KatipContext (KatipLogger m) where
  getKatipContext = view (katipConfigL . contextsL)
  localKatipContext modifyContexts (KatipLogger action) =
    KatipLogger $
      local (over (katipConfigL . contextsL) modifyContexts) action

  getKatipNamespace = view (katipConfigL . namespaceL)
  localKatipNamespace modifyNamespace (KatipLogger action) =
    KatipLogger $
      local (over (katipConfigL . namespaceL) modifyNamespace) action

--------------------------------------------------------------------------------

-- | Configuration record for a 'Katip'-powered 'Logger'.
--
-- While not directly used by this module, it shall be used to power the
-- logging interface defined above up by 'Katip' "behind the scenes".
data KatipConfig = KatipConfig
  { contexts :: LogContexts,
    logEnv :: LogEnv,
    namespace :: Namespace
  }

contextsL :: Lens' KatipConfig LogContexts
contextsL = lens getter setter
  where
    getter KatipConfig {contexts} = contexts
    setter cfg contexts = cfg {contexts}

logEnvL :: Lens' KatipConfig LogEnv
logEnvL = lens getter setter
  where
    getter KatipConfig {logEnv} = logEnv
    setter cfg logEnv = cfg {logEnv}

namespaceL :: Lens' KatipConfig Namespace
namespaceL = lens getter setter
  where
    getter KatipConfig {namespace} = namespace
    setter cfg namespace = cfg {namespace}

-- | Convenience alias for expressing that a given @environment@ can retrieve
-- and modify some 'KatipConfig'.
type WithKatipConfig env m =
  ( HasKatipConfig env,
    MonadReader env m
  )

-- | Interface describing how a 'KatipConfig' can be retrieved and modified
-- from some larger application configuration environment.
class HasKatipConfig env where
  katipConfigL :: Lens' env KatipConfig

  getKatipConfig :: env -> KatipConfig
  getKatipConfig = view katipConfigL
