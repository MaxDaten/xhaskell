{-# LANGUAGE RecordWildCards #-}

module ServerSettings where

import System.Environment.Blank (getEnvDefault)

envPrefix :: String
envPrefix = "XHASKELL_"

data ServerSettings = ServerSettings
  { port :: Int,
    staticDir :: FilePath
  }
  deriving (Show)

defaultServerSettings :: ServerSettings
defaultServerSettings =
  ServerSettings
    { port = 8080,
      staticDir = "app/static"
    }

devServerSettings :: ServerSettings
devServerSettings =
  ServerSettings
    { port = 4242,
      staticDir = "app/static"
    }

fromEnv :: ServerSettings -> IO ServerSettings
fromEnv defaultSettings = do
  port <- read <$> getEnvDefault (envPrefix <> "PORT") (show (port defaultSettings))
  staticDir <- getEnvDefault (envPrefix <> "STATIC_DIR") (staticDir defaultSettings)
  return (ServerSettings {..})
