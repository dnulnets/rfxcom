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
import           System.Hardware.Serialport (CommSpeed (..),
                                             SerialPortSettings (..),
                                             defaultSerialSettings, hOpenSerial)

import           Control.Monad

import           Pipes
import qualified Pipes.Binary               as PB (DecodingError (..),
                                                   decodeGet)
import qualified Pipes.ByteString           as PBS (hGetSome)
import qualified Pipes.Parse                as PP
import qualified Pipes.Prelude              as P (mapM_)
import           Pipes.Safe

import           System.Hardware.Serialport
import           System.IO

--
-- Internal import section
--
import           RFXCom.Message.Base        (Message)
import           RFXCom.Message.Decoder     (msgParser)

--
-- Main to test the pipe sequence
--
main :: IO ()
main = do
     tmp <- processSerialPort $ liftIO . print
     return ()

-- |Open up the serial port with the correct settings for communicating with an
-- RFXCOM device.
openMySerial :: IO Handle -- ^The serial port handle
openMySerial = hOpenSerial "/dev/ttyUSB0" defaultSerialSettings { commSpeed = CS38400,
                                                                  bitsPerWord = 8,
                                                                  stopb = One,
                                                                  parity = NoParity,
                                                                  timeout = 10}

-- |Run the pipes.
processSerialPort ::
     (MonadIO m, MonadMask m)
  => ((Either PB.DecodingError Message) -> m ()) -- ^The message handler function
  -> m () -- ^The result of the pipe execution session
processSerialPort handler =
  runEffect . runSafeP $ do
    serial <- liftIO openMySerial
    forever $ PBS.hGetSome 1 serial
    >-> PP.parseForever msgParser
    >-> P.mapM_ (lift . handler)
