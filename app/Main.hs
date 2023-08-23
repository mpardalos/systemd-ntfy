{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Main where

import Control.Concurrent (threadDelay)
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
import DBus.TH.EDSL
import Data.Map (Map)
import Data.Map qualified as Map
import Safe (fromJustNote)

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

main :: IO ()
main = do
  client <- connectSystem

  unitObject <-
    fromJustNote "Could not find unit 'tailscaled.service'"
      <$> getUnit client "tailscaled.service"

  systemdSubscribe client
  void $ onPropertiesChanged client unitObject $ \_ changedProperties _ ->
    case fromVariant =<< Map.lookup "ActiveState" changedProperties of
      Just activeState -> putStrLn ("Active state for tailscaled is " ++ activeState)
      Nothing -> pure ()

  forever (threadDelay 1_000_000)
