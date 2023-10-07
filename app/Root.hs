{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Root where

import Component.BodyWrapper (bodyWrapper)
import Component.Prompt (promptView)
import Lucid

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
    bodyWrapper $ do
      promptView "How do I get started with Haskell?"

tailwind :: (Monad m) => HtmlT m ()
tailwind = script_ [src_ "https://cdn.tailwindcss.com/3.3.3"] ("" :: String)

htmx :: (Monad m) => HtmlT m ()
htmx = script_ [src_ "https://unpkg.com/htmx.org@1.9.6", integrity_ "sha384-FhXw7b6AlE/jyjlZH5iHa/tTe9EpJ1Y55RjcgPbjeWMskSxZt1v9qkxLJWNJaGni", crossorigin_ "anonymous"] ("" :: String)
