{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Capabilities.Logger.Katip
  ( -- * Logging capability

    -- ** Typeclass interface
    Logger (..),

    -- *** "Production" logger
    ProductionLogger (..),

    -- *** Katip-based Logger instance carrier
    KatipLogger (..),

    -- * "Real-world" Katip logger instance carrier
    KatipM (..),

    -- ** "Real-world" Katip logger configuration
    KatipConfig (..),
    HasKatipConfig (..),
    contextsL,
    logEnvL,
    namespaceL,
  )
where

import Capabilities.Logger (Logger (..))
import Data.Kind (Type)
import GHC.Exception (SrcLoc (..))
import GHC.Stack (getCallStack)
import Katip
  ( Katip (..),
    KatipContext (..),
    LogContexts,
    LogEnv,
    LogStr (..),
    Namespace,
    Severity (..),
    logItemM,
  )
import Katip.Monadic (NoLoggingT (..))
import Language.Haskell.TH.Syntax (Loc (..))
import RIO
import RIO.List (headMaybe)

--------------------------------------------------------------------------------

-- | A 'ProductionLogger' acts as a "carrier" for all of the instances required
-- to implement a 'Logger' backed by the @katip@ library.
--
-- In effect, this is a convenience type which only serves to compose the
-- implementations of 'KatipLogger' and 'KatipM' when adding this capability
-- to an application's execution context.
type ProductionLogger :: Type -> (Type -> Type) -> Type -> Type
newtype ProductionLogger env m result = ProductionLogger (m result)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader env) via m
  deriving (Katip, KatipContext) via (KatipM env (ProductionLogger env m))
  deriving (Logger) via (KatipLogger (ProductionLogger env m))

--------------------------------------------------------------------------------

-- | A 'KatipLogger' acts as a "carrier" for 'Logger' instances implemented in
-- terms of the 'Katip' and 'KatipContext' typeclasses.
--
-- This makes it possible to derive all the capabilities provided by a 'Logger'
-- in terms of some type's underlying 'Katip' and 'KatipContext' instances.
type KatipLogger :: (Type -> Type) -> Type -> Type
newtype KatipLogger m result = KatipLogger (m result)
  deriving newtype (Functor, Applicative, Monad, MonadIO, Katip, KatipContext)

-- | All 'KatipLogger's fulfill the logging interface described by 'Logger'.
instance (Monad m, KatipContext m) => Logger (KatipLogger m) where
  debug = logItemM (toLoc ?callStack) DebugS . LogStr
  info = logItemM (toLoc ?callStack) InfoS . LogStr
  warn = logItemM (toLoc ?callStack) WarningS . LogStr
  error = logItemM (toLoc ?callStack) ErrorS . LogStr

-- | Try to extract the last callsite from some GHC 'CallStack' and convert it
-- to a 'Loc' so that it can be logged with 'logItemM'.
toLoc :: CallStack -> Maybe Loc
toLoc stk =
  let mLoc = headMaybe . reverse $ getCallStack stk
   in mLoc <&> \(_, loc) ->
        Loc
          { loc_filename = srcLocFile loc,
            loc_package = srcLocPackage loc,
            loc_module = srcLocModule loc,
            loc_start = (srcLocStartLine loc, srcLocStartCol loc),
            loc_end = (srcLocEndLine loc, srcLocEndCol loc)
          }

--------------------------------------------------------------------------------

-- | 'KatipM' as a "carrier" for 'Katip' and 'KatipContext' instances
-- implemented in terms of 'IO' an some environment that can provide them with
-- their required configuration information.
type KatipM :: Type -> (Type -> Type) -> Type -> Type
newtype KatipM env m result = KatipM (m result)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader env) via m

instance (MonadIO m, WithKatipConfig env m) => Katip (KatipM env m) where
  getLogEnv = view (katipConfigL . logEnvL)
  localLogEnv modifyLogEnv (KatipM action) =
    KatipM $
      local (over (katipConfigL . logEnvL) modifyLogEnv) action

instance (MonadIO m, WithKatipConfig env m) => KatipContext (KatipM env m) where
  getKatipContext = view (katipConfigL . contextsL)
  localKatipContext modifyContexts (KatipM action) =
    KatipM $
      local (over (katipConfigL . contextsL) modifyContexts) action

  getKatipNamespace = view (katipConfigL . namespaceL)
  localKatipNamespace modifyNamespace (KatipM action) =
    KatipM $
      local (over (katipConfigL . namespaceL) modifyNamespace) action

--------------------------------------------------------------------------------

-- | Configuration record for a 'Katip'-powered 'Logger'.
--
-- While not directly used by this module, it powers the logging interface
-- interface defined above up by 'Katip' "behind the scenes".
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
