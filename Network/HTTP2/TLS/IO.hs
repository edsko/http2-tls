{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.TLS.IO where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import Network.Socket.BufferPool
import qualified Network.Socket.ByteString as NSB
import Network.TLS hiding (HostName)
import System.IO.Error (isEOFError)
import qualified UnliftIO.Exception as E

-- HTTP2: confReadN == recvTLS
-- TLS:   recvData  == contextRecv == backendRecv

----------------------------------------------------------------

mkRecvTCP :: Socket -> IO (IO ByteString)
mkRecvTCP sock = do
    pool <- newBufferPool 2048 16384
    return $ receive sock pool

sendTCP :: Socket -> ByteString -> IO ()
sendTCP sock = NSB.sendAll sock

----------------------------------------------------------------

sendTLS :: Context -> ByteString -> IO ()
sendTLS ctx = sendData ctx . LBS.fromStrict

sendManyTLS :: Context -> [ByteString] -> IO ()
sendManyTLS ctx = sendData ctx . LBS.fromChunks

-- TLS version of recv (decrypting) without a cache.
recvTLS :: Context -> IO ByteString
recvTLS ctx = E.handle onEOF $ recvData ctx
  where
    onEOF e
        | Just Error_EOF <- E.fromException e = return ""
        | Just ioe <- E.fromException e, isEOFError ioe = return ""
        | otherwise = E.throwIO e

----------------------------------------------------------------

mkBackend :: Socket -> IO Backend
mkBackend sock = do
    let send = sendTCP sock
    recv <- mkRecvTCP sock
    recvN <- makeRecvN "" recv
    return Backend
        { backendFlush = return ()
        , backendClose = gracefulClose sock 5000 `E.catch` \(E.SomeException _) -> return ()
        , backendSend  = send
        , backendRecv  = recvN
        }
