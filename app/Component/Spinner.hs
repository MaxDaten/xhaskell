{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Component.Spinner where

import Lucid
import Lucid.Base (makeElement)
import Template (htmlTemplate)

spinner :: (Monad m) => [Attributes] -> HtmlT m ()
spinner attributes = do
  span_
    ( [ class_ "w-16 w-16 flex justify-center p-4 transition-opacity duration-200 ease-in-out"
      ]
        <> attributes
    )
  $ do
    let classes = "fill-red-800" :: String
    toHtmlRaw ([htmlTemplate|blocks-wave.svg|] :: String)

use_ :: (Monad m) => [Attributes] -> HtmlT m ()
use_ attributes = makeElement "use" attributes ""
