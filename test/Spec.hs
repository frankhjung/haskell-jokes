{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Can encode and decode when ByteString does not contain special characters like
'°' (ASCII decimal \176).
-}

module Main ( main
            , jokeResponse
            , jokeString
            , jokeValue
            ) where

import           Data.Aeson                    (Value (..), eitherDecode,
                                                encode, object, (.=))
-- import           Data.Aeson.QQ.Simple
import           Data.ByteString.Lazy.Internal (ByteString (..))
import           Test.Hspec                    (describe, hspec, it, shouldBe)
-- import           Test.Hspec.Expectations.Json  (shouldBeJson)

import           Joke                          (JokeResponse (..))

-- | An instance of 'Joke'.
jokeResponse :: JokeResponse
jokeResponse = JokeResponse { _type = "general"
                            , _setup = "What do you call a pig with three eyes?"
                            , _punchline = "Piiig"
                            , _id = 216
                            }

-- | A JSON string representing a 'Joke'.
jokeString :: ByteString
jokeString = "{\"type\":\"general\",\
             \\"setup\":\"What do you call a pig with three eyes?\",\
             \\"punchline\":\"Piiig\",\
             \\"id\":216}"

-- | Encode a 'Joke' from a Value.
jokeValue :: Value
jokeValue = object [ "type" .= String "general"
                   , "setup" .= String "What do you call a pig with three eyes?"
                   , "punchline" .= String "Piiig"
                   , "id" .= Number 216
                   ]

-- | Unit Tests
main :: IO ()
main = hspec $ do
  describe "test decode joke" $
    it "returns an instance of JokeResponse" $
      eitherDecode jokeString `shouldBe` Right jokeResponse
  describe "test encode joke from Value" $
    it "returns a JSON string" $
      encode jokeValue `shouldBe` encode jokeResponse
