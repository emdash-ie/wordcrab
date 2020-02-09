module Main where

import System.Random (getStdGen)
import System.Random.Shuffle (shuffle')

main :: IO ()
main = do
  gen <- getStdGen
  let things = [1..10]
  print $ shuffle' things (length things) gen
