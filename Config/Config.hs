module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import Web.View.CustomCSSFramework
import qualified IHP.Log as Log
import IHP.Log.Types

config :: ConfigBuilder
config = do
    -- See https://ihp.digitallyinduced.com/Guide/config.html
    -- for what you can do here
    option customTailwind

    -- Less verbose logs.
    logger <- liftIO $ newLogger def
      { level = Warn
      , formatter = withTimeFormatter
      }
    option logger