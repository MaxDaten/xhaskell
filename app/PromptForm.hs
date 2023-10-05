{-# LANGUAGE OverloadedStrings #-}

module PromptForm where

import Data.Functor.Identity (Identity)
import Lucid

promptForm :: HtmlT Identity ()
promptForm = do
  form_ [class_ "flex flex-col border-gray-300"] $ do
    input_ [class_ "border-2 border-gray-300 p-2 rounded-lg", type_ "text", placeholder_ "Enter your question"]
    button_ [class_ "bg-blue-500 text-white p-2 rounded-lg mt-2", type_ "submit"] "Ask"
