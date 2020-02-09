module Scrabble where

import System.Random (getStdGen)
import System.Random.Shuffle (shuffle')

main :: IO ()
main = do
  gen <- getStdGen
  let things = "hello there!"
  print $ shuffle' things (length things) gen
