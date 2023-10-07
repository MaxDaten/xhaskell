{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Component.Prompt (PromptAPI, promptHandler)
import Data.Functor.Identity (Identity)
import Lucid
import Network.Wai.Handler.Warp (run)
import Root (root)
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
import ServerSettings (ServerSettings (..), defaultServerSettings, devServerSettings, fromEnv)
import Text.Printf (printf)

type API =
  "index.html" :> Get '[HTML] (HtmlT Identity ())
    :<|> PromptAPI
    :<|> Raw

api :: Proxy API
api = Proxy

server :: Server API
server =
  return root
    :<|> promptHandler
    :<|> serveDirectoryWebApp "app/static"

app :: Application
app = serve api server

main :: IO ()
main = mainWithSettings =<< fromEnv defaultServerSettings

mainForDevelopment :: IO ()
mainForDevelopment = mainWithSettings =<< fromEnv devServerSettings

mainWithSettings :: ServerSettings -> IO ()
mainWithSettings ServerSettings {..} = do
  printf "Running on port %d\n" port
  printf "Visit: http://localhost:%d/index.html\n" port
  run port app
