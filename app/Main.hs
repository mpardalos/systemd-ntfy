{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.String (IsString (..))
import Network.Wreq (post)
import System.Environment (getArgs)
import SystemD (connectSystem, monitorService)
import Text.Printf (printf)

notify :: ByteString -> IO ()
notify message = void $ post "http://ntfy.sh/elf-goblin-alerts" message

main :: IO ()
main = do
  serviceNames <- getArgs

  client <- connectSystem

  for_ serviceNames $ \serviceName -> do
    mHandler <- monitorService client serviceName $ \activeState -> do
      let msg = printf "Active state for %s is %s" serviceName activeState
      putStrLn msg
      notify (fromString msg)

    case mHandler of
      Nothing -> printf "Could not monitor service %s\n" serviceName
      _ -> pure ()

  forever (threadDelay 1_000_000)
