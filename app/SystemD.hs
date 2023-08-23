{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use forM_" #-}

module SystemD
  ( systemdSubscribe,
    monitorService,
    -- * Convenience re-exports
    connectSystem,
    Client
  )
where

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
onPropertiesChanged client objectPath f = do
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

-- | Run an IO action whenever the ActiveState of a service changes.
-- Use the full name of the service (i.e. 'httpd.service', not 'httpd')
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
