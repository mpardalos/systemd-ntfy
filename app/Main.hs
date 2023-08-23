{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.String (IsString (..))
import Foreign (allocaArray0)
import Foreign.C (CInt (..), CSize (..), CString, peekCString, throwErrnoIfMinus1_)
import Network.Wreq (post)
import System.Environment (getArgs)
import SystemD (connectSystem, monitorService, systemdSubscribe)
import Text.Printf (printf)

notify :: ByteString -> IO ()
notify message = void $ post "http://ntfy.sh/elf-goblin-alerts" message

main :: IO ()
main = do
  serviceNames <- getArgs
  hostname <- getHostName

  client <- connectSystem

  systemdSubscribe client

  for_ serviceNames $ \serviceName -> do
    mHandler <- monitorService client serviceName $ \activeState -> do
      notify (fromString $ printf "%s: Service %s state is %s" hostname serviceName activeState)

    case mHandler of
      Nothing -> printf "Could not monitor service %s\n" serviceName
      _ -> pure ()

  forever (threadDelay 1_000_000)

foreign import ccall "gethostname"
  c_gethostname :: CString -> CSize -> IO CInt

getHostName :: IO String
getHostName = do
  let size = 256
  allocaArray0 size $ \cstr -> do
    throwErrnoIfMinus1_ "getHostName" $ c_gethostname cstr (fromIntegral size)
    peekCString cstr
