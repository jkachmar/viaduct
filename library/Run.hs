module Run (run) where

import App (Config (..), runApp)
import Capabilities.Logger (KatipConfig (..), Logger)
import Capabilities.Logger qualified as Log
import Katip (LogContexts, LogEnv, Namespace)
import RIO

-------------------------------------------------------------------------------

-- | Example effectful function within our application, only defined in terms
-- of the typeclass interfaces which define the capabilities we wish for it to
-- have access to.
--
-- Note that we /cannot/ perform arbitrary 'IO' actions here, as we have no
-- access to 'IO' via 'MonadIO', 'MonadUnliftIO', etc.
app :: (Logger m) => m ()
app = do
  -- This fails to compile!
  -- liftIO $ putStrLn "Hello!"

  -- Example HTTP calls...
  -- result :: Text <- Http.call undefined undefined

  Log.info "Everything's fine!"

--------------------------------------------------------------------------------

-- | Main entry point for our application where we performan configuration
run :: IO ()
run = do
  -- Set up Katip logging configuration, these may be 'IO' actions
  contexts :: LogContexts <- undefined
  logEnv :: LogEnv <- undefined
  namespace :: Namespace <- undefined
  let -- Assemble logging configuration.
      katipConfig :: KatipConfig = KatipConfig {contexts, logEnv, namespace}
      -- Assemble application-wide configuration.
      config = Config {katipConfig}

  -- Run the application
  runApp config app
