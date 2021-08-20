{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module Run (run) where

import App (Config (..), runApp)
import Capabilities.Logger qualified as Log
import Capabilities.Logger.Katip (KatipConfig (..), Logger)
import Katip qualified as Katip
import Formatting ((%), bformat, int)
import RIO

-------------------------------------------------------------------------------

-- | Example function that performs some effects, only defined in terms of the
-- typeclass interfaces which define the capabilities we wish for it to have
-- access to.
--
-- Note that we /cannot/ perform arbitrary 'IO' actions here, as we have no
-- access to 'IO' via 'MonadIO', 'MonadUnliftIO', etc.
app :: (Logger m) => m ()
app = do
  -- This fails to compile!
  -- liftIO $ putStrLn "Hello!"

  -- Example HTTP calls...
  -- result :: Text <- Http.call undefined undefined

  Log.info $ bformat ("Number of requests served: " % int) (0 :: Int)
  Log.debug $ bformat ("Number of requests served: " % int) (0 :: Int)

--------------------------------------------------------------------------------

-- | Main entry point for our application where we assemble the configuration
-- and run the application.
run :: IO ()
run = do
  bracket (mkLogEnv Katip.jsonFormat) Katip.closeScribes \logEnv -> do
    let -- Assemble logging configuration.
        contexts = mempty
        namespace = mempty
        katipConfig :: KatipConfig = KatipConfig {contexts, logEnv, namespace}
        -- Assemble application-wide configuration.
        config = Config {katipConfig}

    -- Run the application.
    runApp config app

-- | Helper function to make a 'Katip.LogEnv' with some reasonable default
-- settings for this application.
mkLogEnv :: (forall a. Katip.LogItem a => Katip.ItemFormatter a) -> IO Katip.LogEnv
mkLogEnv formatter = do
  handleScribe <-
    Katip.mkHandleScribeWithFormatter
      formatter
      Katip.ColorIfTerminal
      stdout
      (Katip.permitItem Katip.DebugS)
      Katip.V2
  Katip.initLogEnv "viaduct" "production" >>= \logEnv ->
    Katip.registerScribe
      "stdout"
      handleScribe
      Katip.defaultScribeSettings
      logEnv
