module ServerSettings where

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