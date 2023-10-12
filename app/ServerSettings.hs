module ServerSettings where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

type Port = Int

data ServerSettings = ServerSettings
  { port :: Port,
    staticDir :: FilePath
  }
  deriving (Show)

serverSettingsWithEnv :: IO ServerSettings
serverSettingsWithEnv =
  ServerSettings
    <$> lookupPort
    <*> lookupStaticDir

lookupPort :: IO Int
lookupPort = read . fromMaybe "8080" <$> lookupEnv "PORT"

lookupStaticDir :: IO FilePath
lookupStaticDir = fromMaybe "app/static" <$> lookupEnv "STATIC_DIR"
