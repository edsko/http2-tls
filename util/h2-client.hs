{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import Network.HTTP2.TLS.Client
import Network.TLS
import System.Console.GetOpt
import System.Environment
import System.Exit

import Network.HTTP2.Client (Path)

import Client
import Monitor

defaultOptions :: Options
defaultOptions =
    Options
        { optPerformance = 0
        , optNumOfReqs = 1
        , optMonitor = False
        , optInteractive = False
        , optKeyLogFile = Nothing
        , optValidate = False
        , optResumption = False
        , opt0RTT = False
        }

usage :: String
usage = "Usage: h2-client [OPTION] addr port [path]"

options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['t']
        ["performance"]
        (ReqArg (\n o -> o{optPerformance = read n}) "<size>")
        "measure performance"
    , Option
        ['n']
        ["number-of-requests"]
        (ReqArg (\n o -> o{optNumOfReqs = read n}) "<n>")
        "specify the number of requests"
    , Option
        ['m']
        ["monitor"]
        (NoArg (\opts -> opts{optMonitor = True}))
        "run thread monitor"
    , Option
        ['i']
        ["interactive"]
        (NoArg (\o -> o{optInteractive = True}))
        "enter interactive mode"
    , Option
        ['l']
        ["key-log-file"]
        (ReqArg (\file o -> o{optKeyLogFile = Just file}) "<file>")
        "a file to store negotiated secrets"
    , Option
        ['e']
        ["validate"]
        (NoArg (\o -> o{optValidate = True}))
        "validate server's certificate"
    , Option
        ['R']
        ["resumption"]
        (NoArg (\o -> o{optResumption = True}))
        "try session resumption"
    , Option
        ['Z']
        ["0rtt"]
        (NoArg (\o -> o{opt0RTT = True}))
        "try sending early data"
    ]

showUsageAndExit :: String -> IO a
showUsageAndExit msg = do
    putStrLn msg
    putStrLn $ usageInfo usage options
    exitFailure

clientOpts :: [String] -> IO (Options, [String])
clientOpts argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
        (_, _, errs) -> showUsageAndExit $ concat errs

main :: IO ()
main = do
    args <- getArgs
    (opts, ips) <- clientOpts args
    (host, port, paths) <- case ips of
        [] -> showUsageAndExit usage
        _ : [] -> showUsageAndExit usage
        h : p : [] -> return (h, read p, ["/"])
        h : p : ps -> return (h, read p, C8.pack <$> ps)
    when (optMonitor opts) $ void $ forkIO $ monitor $ threadDelay 1000000
    ref <- newIORef Nothing
    let keylog = case optKeyLogFile opts of
            Nothing -> settingsKeyLogger defaultSettings
            Just file -> \msg -> appendFile file (msg ++ "\n")
        settings =
            defaultSettings
                { settingsValidateCert = optValidate opts
                , settingsKeyLogger = keylog
                , settingsSessionManager = sessionRef ref
                }
    run settings host port $ client' opts paths
    when (optResumption opts || opt0RTT opts) $ do
        mr <- readIORef ref
        case mr of
            Nothing -> do
                putStrLn "No session data"
                exitFailure
            _ -> do
                let settings2 =
                        defaultSettings
                            { settingsValidateCert = optValidate opts
                            , settingsKeyLogger = keylog
                            , settingsWantSessionResume = mr
                            , settingsUseEarlyData = opt0RTT opts
                            }
                run settings2 host port $ client opts paths

sessionRef :: IORef (Maybe (SessionID, SessionData)) -> SessionManager
sessionRef ref =
    noSessionManager
        { sessionEstablish = \sid sdata -> do
            writeIORef ref $ Just (sid, sdata)
            return Nothing
        }

client' :: Options -> [Path] -> Client ()
client' opts paths sendRequest _aux
    | optInteractive opts = do
        let action = client opts paths sendRequest _aux
        console opts paths action _aux
        return ()
    | otherwise = client opts paths sendRequest _aux
