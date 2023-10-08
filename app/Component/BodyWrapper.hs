{-# LANGUAGE OverloadedStrings #-}

module Component.BodyWrapper where

import Lucid

bodyWrapper :: (Monad m) => HtmlT m () -> HtmlT m ()
bodyWrapper content =
  body_ [class_ "bg-gray-200"] $ do
    header_ [class_ "text-center py-16 bg-blue-500 text-white"] $
      h1_ [class_ "text-4xl"] "Ask Anyting!"
    main_ [class_ "flex flex-col justify-center mt-10"] $ do
      div_ [class_ "flex justify-center"] $ do
        div_ [class_ "w-1/2 border rounded border-gray-400 px-16 py-8 bg-gray-300"] $ do
          content
