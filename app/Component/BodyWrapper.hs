{-# LANGUAGE OverloadedStrings #-}

module Component.BodyWrapper where

import Lucid

bodyWrapper :: (Monad m) => HtmlT m () -> HtmlT m ()
bodyWrapper content =
  body_ [class_ "bg-gradient-to-br from-gray-900/50 via-blue-900/50 to-blue-950/50 w-full h-screen"] $ do
    header_ [class_ "text-center py-16 text-white"] $
      h1_ [class_ "text-4xl font-black font-serif text-purple-500"] $
        do span_ [class_ "text-purple-200 underline"] "Ask" <> span_ " Anyting!"
    main_ [class_ "flex flex-col justify-center my-10"] $ do
      div_ [class_ "flex items-center min-w-full justify-center"] $ do
        div_ [class_ "w-2/3 max-w-xl border rounded-3xl bg-violet-500 bg-gradient-to-br from-purple-600 via-violet-600 to-indigo-600 border-gray-400 md:px-8 md:py-8 py-4 px-4 shadow-indigo-500/50 shadow-lg ring-1 ring-inset ring-white/20"] $ do
          content
