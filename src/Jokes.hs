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

module Jokes ( -- | Functions
               getJoke
             , tellJoke
               -- | Types
             , Joke (..)
             ) where

import           Data.Aeson              (FromJSON, Options (..), ToJSON,
                                          defaultOptions, eitherDecode,
                                          genericParseJSON, genericToJSON,
                                          parseJSON, toJSON)
import           Data.Text               (Text, unlines, unpack)
import           GHC.Generics            (Generic)
import           Network.HTTP.Client     (httpLbs, newManager, parseRequest_,
                                          responseBody)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Prelude                 hiding (unlines)

-- | A response from the joke web site.
data Joke = Joke
    { _type      :: Text -- ^ type of joke
    , _setup     :: Text -- ^ setup the joke
    , _punchline :: Text -- ^ punchline
    , _id        :: Int  -- ^ identifier
    } deriving (Generic, Show, Eq)

-- | Use generic instance to parse JSON.
-- The default instance would parse the fields as @type@, @setup@, @punchline@.
instance FromJSON Joke where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

-- | Use generic instance to encode JSON.
-- The default instance would encode the fields as @type@, @setup@, @punchline@.
instance ToJSON Joke where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

-- | Read joke from web site.
--  The Either type is used for error handling, so we can return error message if
--  something went wrong.
getJoke :: IO (Either String Joke)  -- ^ Joke response or error message
getJoke = do
  let url = "https://official-joke-api.appspot.com/random_joke"
  manager <- newManager tlsManagerSettings
  response <- httpLbs (parseRequest_ url) manager
  return $ eitherDecode $ responseBody response

-- | Print the setup and punchline of a joke.
tellJoke :: Joke          -- ^ `Joke`
            -> String     -- ^ formatted joke as setup and punchline
tellJoke = unpack . unlines . sequence [_setup, _punchline]
