{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Component.Prompt (PromptAPI, promptHandler, promptView)
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

root :: (Monad m) => HtmlT m ()
root = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "Ask SO"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "style.css"]
      link_ [rel_ "manifest", href_ "site.webmanifest"]
      tailwind
      htmx
    body_ [class_ "bg-gray-200"] $ do
      header_ [class_ "text-center py-16 bg-blue-500 text-white"] $
        h1_ [class_ "text-4xl"] "Ask SO"
      main_ [class_ "flex flex-col justify-center mt-10"] $ do
        div_ [class_ "flex justify-center"] $ do
          div_ [class_ "w-1/2 border rounded border-gray-400 px-16 py-8 bg-gray-300"] $ do
            promptView "How do I get started with Haskell?"

tailwind :: (Monad m) => HtmlT m ()
tailwind = script_ [src_ "https://cdn.tailwindcss.com/3.3.3"] ("" :: String)

htmx :: (Monad m) => HtmlT m ()
htmx = script_ [src_ "https://unpkg.com/htmx.org@1.9.6", integrity_ "sha384-FhXw7b6AlE/jyjlZH5iHa/tTe9EpJ1Y55RjcgPbjeWMskSxZt1v9qkxLJWNJaGni", crossorigin_ "anonymous"] ("" :: String)
