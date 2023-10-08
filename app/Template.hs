{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Template where

import Data.String.Here (template)
import Language.Haskell.TH.Quote

templateDirectory :: String
templateDirectory = "app/templates/"

htmlTemplate :: QuasiQuoter
htmlTemplate =
  QuasiQuoter
    { quoteExp = \templateFile -> do
        let path = templateDirectory ++ templateFile
        quoteExp template path
    }
