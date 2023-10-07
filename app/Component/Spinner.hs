{-# LANGUAGE OverloadedStrings #-}

module Component.Spinner where

import Lucid

spinner :: (Monad m) => [Attributes] -> HtmlT m ()
spinner attribs = do
  div_
    ( [ class_ "fill-slate-500 flex justify-center htmx-indicator"
      ]
        <> attribs
    )
    $ do
      img_ [class_ "fill-slate-500 stroke-slate-500", src_ "/blocks-wave.svg", alt_ "Loading..."]