{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}
{-# HLINT ignore "Use forM_" #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import DBus
  ( IsVariant (fromVariant),
    MethodCall (methodCallDestination),
    Signal (..),
    methodCall,
  )
import DBus.Client
  ( MatchRule (..),
    SignalHandler,
    addMatch,
    getProperty,
    matchAny,
  )
import DBus.Internal.Types (Value)
import DBus.TH.EDSL
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString (..))
import Network.Wreq (post)
import Safe (fromJustNote)
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)

type UnitObject = ObjectPath

getObjectProperty :: IsVariant a => Client -> BusName -> ObjectPath -> InterfaceName -> MemberName -> IO (Maybe a)
getObjectProperty client busName objPath iface property = do
  getProperty client ((methodCall objPath iface property) {methodCallDestination = Just busName})
    >>= \case
      Right (fromVariant -> Just val) -> return (Just val)
      _ -> return Nothing

getStatus :: Client -> UnitObject -> IO (Maybe String)
getStatus client unitObject =
  getObjectProperty
    client
    "org.freedesktop.systemd1"
    unitObject
    "org.freedesktop.systemd1.Unit"
    "ActiveState"

interface'
  "org.freedesktop.systemd1"
  (Just "/org/freedesktop/systemd1")
  "org.freedesktop.systemd1.Manager"
  Nothing
  [ "GetUnit" =:: ''String :-> Return ''ObjectPath,
    "Subscribe" =:: Return ''() `as` "systemdSubscribe"
  ]

onPropertiesChanged :: Client -> ObjectPath -> (InterfaceName -> Map String Variant -> [String] -> IO ()) -> IO SignalHandler
onPropertiesChanged client objectPath f =
  addMatch
    client
    ( matchAny
        { matchPath = Just objectPath,
          matchInterface = Just "org.freedesktop.DBus.Properties",
          matchMember = Just "PropertiesChanged"
        }
    )
    $ \signal ->
      case signal.signalBody of
        [ fromVariant -> Just interfaceName,
          fromVariant -> Just changedProperties,
          fromVariant -> Just invalidatedProperties
          ] -> f interfaceName changedProperties invalidatedProperties
        _ -> error "Unexpected value for PropertiesChanged signal"

monitorService :: Client -> String -> (String -> IO ()) -> IO (Maybe SignalHandler)
monitorService client serviceName f = do
  getUnit client serviceName >>= \case
    Just unitObject -> do
      handler <- onPropertiesChanged client unitObject $ \_ changedProperties _ ->
        case fromVariant @String =<< Map.lookup "ActiveState" changedProperties of
          Just activeState -> f activeState
          Nothing -> pure ()
      return (Just handler)
    Nothing -> return Nothing

notify :: ByteString -> IO ()
notify message = void $ post "http://ntfy.sh/elf-goblin-alerts" message

main :: IO ()
main = do
  serviceNames <- getArgs

  client <- connectSystem

  systemdSubscribe client

  for_ serviceNames $ \serviceName -> do
    mHandler <- monitorService client serviceName $ \activeState -> do
      let msg = printf "Active state for %s is %s" serviceName activeState
      putStrLn msg
      notify (fromString msg)

    case mHandler of
      Nothing -> printf "Could not monitor service %s\n" serviceName
      _ -> pure ()

  forever (threadDelay 1_000_000)
