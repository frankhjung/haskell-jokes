{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{- Print a random joke from <https://official-joke-api.appspot.com/random_joke>. -}

module Main (main) where

import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)

import           Jokes        (Joke (..), getJoke)

tellJoke :: Joke -> IO ()
tellJoke joke = putStrLn (_setup joke) >> putStrLn (_punchline joke)

main :: IO ()
main = getJoke >>= either (print . ("Unable to get joke: " <>)) tellJoke
