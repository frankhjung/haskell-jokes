{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{- Print a random joke from <https://official-joke-api.appspot.com/random_joke>. -}

module Main (main) where

import           Jokes (getJoke, tellJoke)

-- | Print the error message from a failed `getJoke` response.
printError :: String -> String
printError = ("Unable to get joke: " <>)

-- | Print a random joke.
main :: IO ()
main = putStrLn . either printError tellJoke =<< getJoke
