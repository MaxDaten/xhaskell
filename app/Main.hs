{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Component.Prompt (PromptAPI, promptHandler)
import Component.Root (root)
import Data.Functor.Identity (Identity)
import Lucid
import Network.Wai.Handler.Warp (run)
import Servant
  ( Application,
    Get,
    Proxy (..),
    Raw,
    Server,
    serve,
    serveDirectoryWebApp,
    (:<|>) (..),
    type (:>),
  )
import Servant.HTML.Lucid (HTML)
import ServerSettings (ServerSettings (..), serverSettingsWithEnv)
import Text.Printf (printf)

type API =
  Get '[HTML] (HtmlT Identity ())
    :<|> "index.html" :> Get '[HTML] (HtmlT Identity ())
    :<|> PromptAPI
    :<|> Raw

api :: Proxy API
api = Proxy

main :: IO ()
main = mainWithSettings =<< serverSettingsWithEnv

mainWithSettings :: ServerSettings -> IO ()
mainWithSettings settings@ServerSettings {..} = do
  printf "%s\n" (show settings)
  printf "Visit: http://localhost:%d/index.html\n" port
  run port (app settings)

server :: ServerSettings -> Server API
server ServerSettings {..} =
  return root
    :<|> return root
    :<|> promptHandler
    :<|> serveDirectoryWebApp staticDir

app :: ServerSettings -> Application
app = serve api . server
