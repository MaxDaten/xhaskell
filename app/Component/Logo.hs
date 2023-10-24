{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Component.Logo where

import Lucid
import Template (htmlTemplate)

brain :: (Monad m) => HtmlT m ()
brain = toHtmlRaw ([htmlTemplate|brain-logo.svg|] :: String)
