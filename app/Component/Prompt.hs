{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Component.Prompt
  ( PromptAPI,
    Prompt (..),
    Answer (..),
    promptHandler,
    promptView,
    answerView,
  )
where

import Component.Spinner (spinner)
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.String (IsString)
import Data.Text (pack)
import Data.UUID (UUID, nil)
import GHC.Generics (Generic)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Servant
import Servant.HTML.Lucid (HTML)
import Text.Printf (printf)
import Web.FormUrlEncoded (FromForm (..))

-- API
------------------

newtype Prompt = Prompt
  { question :: String
  }
  deriving (Eq, Show, Generic, Semigroup, Monoid, IsString)

data Answer = Answer
  { uuid :: UUID,
    prompt :: Prompt,
    answer :: String
  }
  deriving (Eq, Show, Generic)

{- ORMOLU_DISABLE -}
type PromptAPI =
  "prompt" :> 
  (
    -- POST
    ReqBody '[FormUrlEncoded] Prompt :> Post '[HTML] Answer :<|>
    -- GET
    Get '[HTML] Prompt
  )
{- ORMOLU_ENABLE -}

promptHandler :: Server PromptAPI
promptHandler = postPrompt :<|> getPrompt
  where
    postPrompt :: Prompt -> Handler Answer
    postPrompt prompt = do
      -- wait for 2 seconds to simulate a long running process
      void $ liftIO (threadDelay 5000000)
      return (Answer {uuid = nil, prompt, answer = "Just start!"})

    getPrompt :: Handler Prompt
    getPrompt = return (Prompt {question = "How do I get started with Haskell?"})

-- Views
------------------

promptView :: (Monad m) => Prompt -> HtmlT m ()
promptView Prompt {..} = do
  h2_ [class_ ""] "Ask stack overflow all your questions!"

  form_
    [ id_ "question-form",
      class_ "flex flex-col border-gray-300",
      hxPost_ "/prompt",
      hxPushUrl_ "true",
      hxIndicator_ "#loading-spinner",
      hxExt_ "debug"
    ]
    $ do
      input_
        [ class_ "border-2 border-gray-300 p-2 rounded-lg",
          name_ "question",
          type_ "text",
          placeholder_ "Enter your question",
          value_ (pack question)
        ]
      button_
        [ class_ "flex items-center justify-center bg-blue-500 text-white p-2 rounded-lg mt-2 hover:bg-blue-600 disabled:cursor-not-allowed disabled:bg-gray-600 transition-colors duration-200 ease-in-out",
          type_ "submit",
          [__|
            on htmx:beforeSend from #question-form
              log "¿Sending question?"
              toggle @disabled on me
              toggle .hidden on #button-text
              toggle .hidden on #loading-spinner
          |]
        ]
        $ do
          span_ [id_ "button-text"] "Ask"
          spinner
            [ id_ "loading-spinner",
              class_ "hidden"
            ]

-- class_ "hidden"
-- ]

answerView :: (Monad m) => Answer -> HtmlT m ()
answerView Answer {..} = do
  div_ [] $ do
    h2_ "Your prompt was:"
    p_ (toHtml (question prompt))

    h2_ "Your answer is:"
    p_ (toHtml answer)

    hr_ []

    answerFooter
  where
    uuidText = pack $ show uuid
    answerFooter = do
      hr_ []
      p_
        [class_ "text-sm text-slate-500"]
        (toHtml (printf "uuid: %s" uuidText :: String))

instance ToHtml Prompt where
  toHtml = promptView
  toHtmlRaw = toHtml

instance ToHtml Answer where
  toHtml = answerView
  toHtmlRaw = toHtml

instance FromForm Prompt