{-# LANGUAGE OverloadedStrings #-}
-- Echo server program
module Server (main) where

import Control.Concurrent (forkFinally)
import Control.Monad (unless, forever, void)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO.Streams.Network (socketToStreams)
import qualified System.IO.Streams as Streams
import qualified Control.Exception as E
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as JSON

import Scrabble (Player)

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "3000"
    E.bracket (open addr) close loop
  where
    resolve port = do
      let hints = defaultHints {
              addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      -- If the prefork technique is not used,
      -- set CloseOnExec for the security reasons.
      fd <- fdSocket sock
      setCloseOnExecIfNeeded fd
      bind sock (addrAddress addr)
      listen sock 10
      return sock
    loop sock = forever $ do
      (conn, peer) <- accept sock
      putStrLn $ "Connection from " ++ show peer
      void $ forkFinally (talk conn) (\_ -> close conn)
    talk sock = do
      putStrLn "Waiting for message…"
      (inputStream', outputStream) <- socketToStreams sock
      inputStream <- Streams.splitOn (== '\US') inputStream'
      mPlayerString <- Streams.read inputStream
      case mPlayerString of
        Just playerString -> case JSON.eitherDecodeStrict playerString :: Either String Player of

          Left message -> putStrLn "Error:" >> putStrLn message
          Right player -> putStrLn "Reponding…"
            >> Streams.write (Just $ BL.toStrict $ JSON.encode player) outputStream
            >> Streams.write (Just "\US") outputStream
            >> talk sock
        Nothing -> putStrLn "End of input"

