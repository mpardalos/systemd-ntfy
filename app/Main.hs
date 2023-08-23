{-# LANGUAGE OverloadedRecordDot #-}
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
import Options.Applicative
import SystemD (connectSystem, monitorService, systemdSubscribe)
import Text.Printf (printf)

data Options = Options
  { endpoint :: String,
    services :: [String]
  }

parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "Send ntfy alerts when SystemD services change state"
        )

    parser =
      Options
        <$> strOption
          ( short 'n'
              <> long "endpoint"
              <> help "The endpoint to send the notification to"
              <> metavar "URL"
          )
        <*> many
          ( strOption
              ( short 's'
                  <> long "service"
                  <> help "Repeated. Units to monitor. Must include the suffix (.service)"
                  <> metavar "SERVICE"
              )
          )

notify :: ByteString -> IO ()
notify message = void $ post "http://ntfy.sh/elf-goblin-alerts" message

main :: IO ()
main = do
  opts <- parseOptions
  hostname <- getHostName

  client <- connectSystem

  systemdSubscribe client

  for_ opts.services $ \serviceName -> do
    mHandler <- monitorService client serviceName $ \activeState -> do
      let msg :: ByteString = fromString $ printf "%s: Service %s state is %s" hostname serviceName activeState
      void $ post opts.endpoint msg

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
