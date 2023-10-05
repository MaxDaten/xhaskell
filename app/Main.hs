{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Functor.Identity (Identity)
import Lucid
import Network.Wai.Handler.Warp (run)
import PromptForm (promptForm)
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

type API =
  "index.html" :> Get '[HTML] (HtmlT Identity ())
    :<|> Raw

api :: Proxy API
api = Proxy

server :: Server API
server =
  return root
    :<|> serveDirectoryWebApp "app/static"

app :: Application
app = serve api server

devPort :: Int
devPort = 4242

main :: IO ()
main = mainWithSettings =<< fromEnv defaultServerSettings

mainForDevelopment :: IO ()
mainForDevelopment = mainWithSettings =<< fromEnv devServerSettings

mainWithSettings :: ServerSettings -> IO ()
mainWithSettings ServerSettings {..} = do
  print ("Running on port " ++ show devPort :: String)
  print ("Visit: http://localhost:4242/index.html" :: String)
  run port app

root :: HtmlT Identity ()
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
        p_ [class_ "w-full"] "Ask stack overflow all your questions!"
        promptForm

tailwind :: HtmlT Identity ()
tailwind = script_ [src_ "https://cdn.tailwindcss.com/3.3.3"] ("" :: String)

htmx :: HtmlT Identity ()
htmx = script_ [src_ "https://unpkg.com/htmx.org@1.9.6", integrity_ "sha384-FhXw7b6AlE/jyjlZH5iHa/tTe9EpJ1Y55RjcgPbjeWMskSxZt1v9qkxLJWNJaGni", crossorigin_ "anonymous"] ("" :: String)
