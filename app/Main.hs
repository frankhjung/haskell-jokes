{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{-

Print a random joke from <https://official-joke-api.appspot.com/random_joke>.

-}

module Main (main) where

import           Joke (JokeResponse (..), getJokeResponse)

main :: IO ()
main = do
  jokeResponse <- getJokeResponse
  case jokeResponse of
    Left e  -> putStrLn $ "Unable to get joke: " <> e
    Right j -> do
      print $ _setup j
      print $ _punchline j
