-- {-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{-

Get a random joke from <https://official-joke-api.appspot.com/random_joke>.

== Example

An example of the data returned by the API:

@
{
  "type": "general",
  "setup": "What do you call a pig with three eyes?",
  "punchline": "Piiig",
  "id": 216
}
@

-}

module Joke ( -- Functions
              getJokeResponse
              -- | Types
            , JokeResponse (..)
            ) where

import           Data.Aeson              (FromJSON, Options (..), ToJSON,
                                          defaultOptions, eitherDecode,
                                          genericToJSON, parseJSON, toJSON,
                                          withObject, (.:))
import           Data.Text               (Text)
import           GHC.Generics            (Generic)
import           Network.HTTP.Client     (httpLbs, newManager, parseRequest_,
                                          responseBody)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

data JokeResponse = JokeResponse
    { _type      :: Text -- ^ type of joke
    , _setup     :: Text -- ^ setup the joke
    , _punchline :: Text -- ^ punchline
    , _id        :: Int  -- ^ identifier
    } deriving (Generic, Show, Eq)

instance FromJSON JokeResponse where
  parseJSON = withObject "JokeResponse" $ \o -> do
    t <- o .: "type"
    s <- o .: "setup"
    p <- o .: "punchline"
    i <- o .: "id"
    return JokeResponse { _type = t
                        , _setup = s
                        , _punchline = p
                        , _id = i
                        }

instance ToJSON JokeResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

-- | Read joke from site.
getJokeResponse :: IO (Either String JokeResponse)  -- joke response
getJokeResponse = do
  let
    url = "https://official-joke-api.appspot.com/random_joke"
    request = parseRequest_ url
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ eitherDecode $ responseBody response
