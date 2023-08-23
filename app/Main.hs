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
  ( BusName,
    InterfaceName,
    IsVariant (fromVariant, toVariant),
    MemberName,
    MethodCall (methodCallBody, methodCallDestination),
    MethodReturn (methodReturnBody),
    ObjectPath,
    Signal (..),
    Variant,
    methodCall,
    typeOf,
  )
import DBus.Client
  ( Client,
    MatchRule (..),
    SignalHandler,
    addMatch,
    call_,
    connectSystem,
    getProperty,
    matchAny,
  )
import DBus.TH.EDSL
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (fromString)
import Safe (atMay, fromJustNote)

newtype UnitObject = UnitObject ObjectPath
  deriving (Show)

class IsObject a where
  getPath :: a -> ObjectPath
  getApplication :: a -> BusName

instance IsObject UnitObject where
  getPath (UnitObject path) = path
  getApplication _ = "org.freedesktop.systemd1"

getObjectProperty :: (IsObject obj, IsVariant a) => Client -> obj -> InterfaceName -> MemberName -> IO (Maybe a)
getObjectProperty client obj iface property = do
  getProperty
    client
    ( (methodCall (getPath obj) iface property)
        { methodCallDestination = Just (getApplication obj)
        }
    )
    >>= \case
      Right (fromVariant -> Just val) -> return (Just val)
      _ -> return Nothing

getStatus :: Client -> UnitObject -> IO (Maybe String)
getStatus client unitObject =
  getObjectProperty
    client
    unitObject
    "org.freedesktop.systemd1.Unit"
    "ActiveState"

interface'
  "org.freedesktop.systemd1"
  (Just "/org/freedesktop/systemd1")
  "org.freedesktop.systemd1.Manager"
  Nothing
  [ "GetUnit" =:: ''String :-> Return ''ObjectPath `as` "getUnitPath",
    "Subscribe" =:: Return ''() `as` "systemdSubscribe"
  ]

getUnit :: Client -> String -> IO (Maybe UnitObject)
getUnit client name = do
  mPath <- getUnitPath client name
  return (UnitObject <$> mPath)

onPropertiesChanged :: IsObject obj => Client -> obj -> (Signal -> IO ()) -> IO SignalHandler
onPropertiesChanged client obj =
  addMatch
    client
    ( matchAny
        { matchPath = Just (getPath obj),
          matchInterface = Just "org.freedesktop.DBus.Properties",
          matchMember = Just "PropertiesChanged"
        }
    )

main :: IO ()
main = do
  client <- connectSystem

  unitObject <-
    fromJustNote "Could not find unit 'tailscaled.service'"
      <$> getUnit client "tailscaled.service"

  systemdSubscribe client
  void $ onPropertiesChanged client unitObject $ \signal -> do
    let changedProperties :: Map String Variant =
          fromJustNote
            "Signal body missing changed properties"
            (fromVariant =<< signal.signalBody `atMay` 1)
    let changedActiveState =
          fromVariant =<< Map.lookup "ActiveState" changedProperties
    case changedActiveState of
      Just activeState -> putStrLn ("Active state for tailscaled is " ++ activeState)
      Nothing -> pure ()
    return ()

  forever (threadDelay 1_000_000)
