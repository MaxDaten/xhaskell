{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Component.Root where

import Component.BodyWrapper (bodyWrapper)
import Component.Prompt (promptView)
import Data.Text (Text)
import Lucid
import Lucid.Hyperscript (useHyperscript)

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
      useHyperscript
      -- https://htmx.org/extensions/
      htmx
        [ "class-tools",
          "debug"
        ]
    bodyWrapper $ do
      promptView "How do I get started with Haskell?"

tailwind :: (Monad m) => HtmlT m ()
tailwind = script_ [src_ "https://cdn.tailwindcss.com/3.3.3"] ("" :: String)

type HtmxExtension = Text

htmx :: (Monad m) => [HtmxExtension] -> HtmlT m ()
htmx extensions =
  script_
    [ src_ "https://unpkg.com/htmx.org@1.9.6",
      integrity_ "sha384-FhXw7b6AlE/jyjlZH5iHa/tTe9EpJ1Y55RjcgPbjeWMskSxZt1v9qkxLJWNJaGni",
      crossorigin_ "anonymous"
    ]
    ("" :: String)
    <> foldMap mkExtensionScriptElement extensions
  where
    mkExtensionScriptElement :: (Monad m) => HtmxExtension -> HtmlT m ()
    mkExtensionScriptElement extension =
      script_
        [ src_ ("https://unpkg.com/htmx.org/dist/ext/" <> extension <> ".js")
        ]
        ("" :: String)
