{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{- Print a random joke from <https://official-joke-api.appspot.com/random_joke>. -}

module Main (main) where

import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)

import           Joke         (JokeResponse (..), getJokeResponse)

main :: IO ()
main = do
  jokeResponse <- getJokeResponse
  case jokeResponse of
    Left err  -> print $ "Unable to get joke: " <> err
    Right joke -> do
      putStrLn $ _setup joke
      putStrLn $ _punchline joke
