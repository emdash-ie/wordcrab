{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Client (main) where

import Scrabble (Player(..), name, tiles, currentScore)

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.IO.Streams.Network (socketToStreams)
import qualified Control.Exception as E
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified System.IO.Streams as Streams

main :: IO ()
main = withSocketsDo $ do
  addr <- resolve "127.0.0.1" "3000"
  E.bracket (open addr) close talk
  where
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock
    talk sock = do
      (inputStream', outputStream) <- socketToStreams sock
      inputStream <- Streams.splitOn (== '\US') inputStream'
      putStrLn "Enter a name (. to finish): "
      line <- getLine
      case line of
        "." -> return ()
        name -> do
          let player = Player { name = name, tiles = [], currentScore = 0 }
          Streams.write (Just $ BL.toStrict $ JSON.encode player) outputStream
          Streams.write (Just "\US") outputStream
          n <- Streams.read inputStream
          case n of
            Just bs ->
              case JSON.eitherDecodeStrict bs :: Either String Player of
                Right player -> print player
                Left errorMessage -> putStrLn "Error:" >> putStrLn errorMessage
            Nothing -> putStrLn "Couldn't read from stream"
          putStr "Raw from stream: "
          print n
          talk sock
