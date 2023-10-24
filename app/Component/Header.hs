{-# LANGUAGE OverloadedStrings #-}

module Component.Header where

import Component.Logo (brain)
import Lucid

header :: (Monad m) => [Attributes] -> HtmlT m ()
header attributes =
  header_ ([class_ "text-center py-16 text-white"] <> attributes) $ do
    span_ [class_ "inline-block w-24 h-24"] brain
    h1_ [class_ "text-4xl font-black font-serif text-purple-500"] $ do
      span_ [class_ "text-purple-200 underline"] "Ask"
      span_ " Anyting!"
