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

import           Control.Concurrent.Chan      (Chan, newChan, readChan,
                                               writeChan)
import           Control.Concurrent.MVar      (MVar, newEmptyMVar, newMVar,
                                               putMVar, takeMVar)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Managed        (Managed, managed, runManaged)

import           Pipes

import qualified Pipes.Binary                 as PB (DecodingError (..),
                                                     decodeGet)
import qualified Pipes.ByteString             as PBS (fromLazy, hGetSome,
                                                      toHandle)
import qualified Pipes.Parse                  as PP
import qualified Pipes.Prelude                as P (mapM_, repeatM)
import           Pipes.Safe

import           Data.ByteString              (ByteString)
import           Data.Word                    (Word8)

--
-- Internal import section
--
import           RFXCom.Message.Base          (Message)
import           RFXCom.Message.Decoder       (msgParser)

import           RFXCom.System.Concurrent     (forkChild, waitForChildren)
import           RFXCom.System.Exception      (ResourceException (..))
import qualified RFXCom.System.Log            as Log (Handle (..), debug, error,
                                                      info, warning)
import           RFXCom.System.Log.FileHandle as LogI (Config (..),
                                                       defaultConfig,
                                                       withHandle)

import qualified RFXCom.Control.RFXComWriter  as RFXComW (Config (..),
                                                          defaultConfig,
                                                          withHandle)

import qualified RFXCom.Control.RFXComReader  as RFXComR (Config (..),
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

--
-- Main to test the pipe sequence
--
main :: IO ()
main = Control.Exception.handle (\(ResourceException s)-> putStrLn $ "Resourceexception: " ++ s) $ do

  serialH <- openMySerial

  runManaged $ do

    loggerH  <- managed $ LogI.withHandle LogI.defaultConfig
    rfxWH    <- managed $ RFXComW.withHandle RFXComW.defaultConfig serialH loggerH
    rfxWR    <- managed $ RFXComR.withHandle RFXComR.defaultConfig serialH loggerH

    liftIO koko

  hClose serialH
  waitForChildren

koko = do
   foo <- putStrLn "RFXCom>"
   name <- getLine
   koko

