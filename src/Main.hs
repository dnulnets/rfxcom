{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This application converts rfxcom messages to and from MQTT messages on an
-- MQTT message broker.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module Main  where

--
-- Import section
--
import           System.Hardware.Serialport   (CommSpeed (..), Parity (..),
                                               SerialPortSettings (..),
                                               StopBits (..),
                                               defaultSerialSettings,
                                               hOpenSerial)
import           System.IO

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Managed        (Managed, managed, runManaged)

--
-- Internal import section
--
import           RFXCom.System.Concurrent     (waitForChildren)
import           RFXCom.System.Exception      (ResourceException (..))

import qualified RFXCom.System.Log.FileHandle as LogI (Config (..),
                                                       defaultConfig,
                                                       withHandle)

import qualified RFXCom.Control.RFXComWriter  as RFXComW (Config (..),
                                                          defaultConfig,
                                                          withHandle)

import qualified RFXCom.Control.RFXComReader  as RFXComR (Config (..),
                                                          defaultConfig,
                                                          withHandle)

import qualified RFXCom.Control.RFXComMaster  as RFXComM (Config (..),
                                                          defaultConfig,
                                                          withHandle)

import qualified RFXCom.Control.RFXComMQTT    as RFXComMQ (Config (..),
                                                           defaultConfig,
                                                           withHandle)

-- |Open up the serial port with the correct settings for communicating with an
-- RFXCOM device.
openMySerial :: IO Handle -- ^The serial port handle
openMySerial = hOpenSerial "/dev/ttyUSB0" defaultSerialSettings { commSpeed = CS38400,
                                                                  bitsPerWord = 8,
                                                                  stopb = One,
                                                                  parity = NoParity,
                                                                  timeout = 10}
-- |The with handler for the serial port functionality
withHandle::(Handle->IO a)  -- ^The IO computation
          ->IO a            -- ^The result of the IO computation
withHandle = Control.Exception.bracket openMySerial hClose

-- |Main function that starts up the entire application and does the dependancy injection
-- to stich it all together.
main :: IO ()
main = Control.Exception.handle (\(ResourceException s)-> putStrLn $ "Resourceexception: " ++ s) $ do

  putStrLn "RFXCom Version 1.0 - RFXCom to MQTT bridge"

  runManaged $ do

    serialH  <- managed $ withHandle
    loggerH  <- managed $ LogI.withHandle LogI.defaultConfig
    rfxWH    <- managed $ RFXComW.withHandle RFXComW.defaultConfig serialH loggerH
    rfxMH    <- managed $ RFXComM.withHandle RFXComM.defaultConfig loggerH rfxWH
    rfxRH    <- managed $ RFXComR.withHandle RFXComR.defaultConfig serialH loggerH rfxMH
    rfxMQ    <- managed $ RFXComMQ.withHandle RFXComMQ.defaultConfig loggerH
    liftIO $ quit

  waitForChildren

-- |Returns when the user presses 'q'
quit::IO ()
quit = do
  hSetBuffering stdin NoBuffering
  loop
  where
    loop = do
      c <- getChar
      if (c /= 'q')
        then loop
        else return ()
