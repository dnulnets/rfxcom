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
import           System.Hardware.Serialport        (CommSpeed (..),
                                                    SerialPortSettings (..),
                                                    defaultSerialSettings,
                                                    hOpenSerial)

import           Control.Concurrent                (threadDelay)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Managed             (Managed, managed,
                                                    runManaged)

import           Pipes
import qualified Pipes.Binary                      as PB (DecodingError (..),
                                                          decodeGet)
import qualified Pipes.ByteString                  as PBS (hGetSome)
import qualified Pipes.Parse                       as PP
import qualified Pipes.Prelude                     as P (mapM_)
import           Pipes.Safe

import           System.Hardware.Serialport
import           System.IO

import           Data.Text                         (Text, pack)

--
-- Internal import section
--
import           RFXCom.Message.Base               (Message)
import           RFXCom.Message.Decoder            (msgParser)
import           RFXCom.System.Concurrent          (forkChild, waitForChildren)
import           RFXCom.System.Exception           (ResourceException (..))
import qualified RFXCom.System.Log                 as Log (Handle (..), debug,
                                                           error, info, warning)

import           RFXCom.System.Log.Impl.FileHandle (Config (..), withHandle)

--
-- Main to test the pipe sequence
--
main :: IO ()
main = Control.Exception.handle (\(ResourceException s)-> putStrLn $ "Resourceexception: " ++ s) $ do
  runManaged $ do
    loggerH <- managed $ withHandle Config
    liftIO $ Log.info loggerH "Hejsan"
    liftIO $ Log.debug loggerH "Hejsan"
    --liftIO $ throwIO ResourceException
    --liftIO $ threadDelay 100000
    liftIO $ processSerialPort $ liftIO . print
  waitForChildren

-- |Open up the serial port with the correct settings for communicating with an
-- RFXCOM device.
openMySerial :: IO Handle -- ^The serial port handle
openMySerial = hOpenSerial "/dev/ttyUSB0" defaultSerialSettings { commSpeed = CS38400,
                                                                  bitsPerWord = 8,
                                                                  stopb = One,
                                                                  parity = NoParity,
                                                                  timeout = 10}

--terminator :: Consumer (Either PB.DecodingError Message) IO ()
terminator :: Consumer (Either PB.DecodingError Message) IO ()
terminator = do
  str <- await
  terminator

take ::  (Monad m) => Int -> Pipe a a (SafeT m) ()
take n = do
    replicateM_ n $ do                     -- Repeat this block 'n' times
        x <- await                         -- 'await' a value of type 'a'
        yield x                            -- 'yield' a value of type 'a'

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
    >-> Main.take 3
    >-> P.mapM_ (lift . handler)

