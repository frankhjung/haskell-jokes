{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{-

Print a random joke from <https://official-joke-api.appspot.com/random_joke>.

-}

module Main (main) where

import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)

import           Joke         (JokeResponse (..), getJokeResponse)

main :: IO ()
main = do
  jokeResponse <- getJokeResponse
  case jokeResponse of
    Left e  -> print $ "Unable to get joke: " <> e
    Right j -> do
      putStrLn $ _setup j
      putStrLn $ _punchline j
