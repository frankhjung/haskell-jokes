{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Still to do:
- How to decode when ByteString contains special characters like '°' (ASCII decimal \176).
- How to decode a list? (e.g. @[JokeResponse]@)
- How to decode a sequence of names objects? (e.g. @{Name1: JokeResponse, Name2: JokeResponse}@)
-}

module Main ( main
            , jokeResponse
            , jokeString
            , jokeValue
            ) where

import           Data.Aeson                    (Value (..), eitherDecode,
                                                encode, object, (.=))
import           Data.ByteString.Lazy.Internal (ByteString (..))
import           Test.Hspec                    (describe, hspec, it, shouldBe)

import           Jokes                         (JokeResponse (..))

-- | A response from web site of a joke.
jokeResponse :: JokeResponse
jokeResponse = JokeResponse { _type = "general"
                            , _setup = "What do you call a pig with three eyes?"
                            , _punchline = "Piiig"
                            , _id = 216
                            }

-- | A JSON string representing a 'JokeResponse'.
jokeString :: ByteString
jokeString = "{\"type\":\"general\",\
             \\"setup\":\"What do you call a pig with three eyes?\",\
             \\"punchline\":\"Piiig\",\
             \\"id\":216}"

-- | A Value instance of a 'JokeResponse'.
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
