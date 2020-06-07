module Main where

import System.Environment (getArgs)

import qualified Wordcrab.Brick as WB
import qualified Wordcrab.Server as WS

main :: IO ()
main = do
  args <- getArgs
  case args of
    "brick" : _ -> WB.main
    "server" : _ -> WS.main
    _ -> putStrLn "Unrecognised args. Try \"brick\" or \"server\"."
