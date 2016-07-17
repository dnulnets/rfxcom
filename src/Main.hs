-- |This application converts rfxcom messages to MQTT messages on a message bus
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
--
module Main  where

--
-- Import section
--
import           Data.Binary
import           Data.Binary.Get
import           Data.ByteString            (ByteString, pack, unpack)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.Maybe
import           Data.Word                  (Word16, Word8)

import           System.Hardware.Serialport (CommSpeed (..),
                                             SerialPortSettings (..),
                                             defaultSerialSettings, hOpenSerial,
                                             send)

import           Control.Applicative
import           Control.Monad

import           Pipes
import qualified Pipes.Binary               as PB (DecodingError (..), decode,
                                                   encode,decodeGet)
import qualified Pipes.ByteString           as PBS (fromLazy, hGetSome)
import qualified Pipes.Parse                as PP
import qualified Pipes.Prelude              as P (mapFoldable, mapM, mapM_)
import           Pipes.Safe

import           System.Hardware.Serialport
import           System.IO

--
-- Generic data Header
--
data Header = Header
  { _size           :: !Word8
  , _type           :: !Word8
  , _subtype        :: !Word8
  , _sequenceNumber :: !Word8
  , _data           :: ![Word8]}
  deriving (Show)

--
-- Main to test the pipe sequence
--
main :: IO ()
main = do
     tmp <- processSerialPort $ liftIO . print
     return ()
     
-- |Validate the body size of the message, it must be more than 3 charatcers, otherwise
-- we have an invalida message so clamp it down to 0 in that case to signal faulty data
-- block size.
validateBodySize::Word8->Word8
validateBodySize x | x>3 = x-3
                   | otherwise = 0

-- |Parses the bytestream into rfxcom messages based on the rfxcom decoder and return with
-- decoding errors or the rfxcom message.
msgParser::(Monad m) => PP.Parser ByteString m (Either () (Either PB.DecodingError Header))
msgParser = do
  msg <- PB.decodeGet msgDecoder  
  return $ Right msg

-- |Describes an rfxcom message decoder.
msgDecoder::Get Header
msgDecoder = do
  msize <- getWord8
  mtype <- getWord8
  msubtype <- getWord8
  msequenceNumber <- getWord8
  mdata <- replicateM (fromIntegral $ validateBodySize msize) getWord8
  return $! Header msize mtype msubtype msequenceNumber mdata
    
-- |Open up the serial port with the correct settings for communicating with RFXCOM
openMySerial :: IO Handle
openMySerial = hOpenSerial "/dev/ttyUSB0" defaultSerialSettings { commSpeed = CS38400,
                                                                  bitsPerWord = 8,
                                                                  stopb = One,
                                                                  parity = NoParity,
                                                                  timeout = 10}

-- |Run the pipes
processSerialPort ::
     (MonadIO m, MonadMask m)
  => ((Either PB.DecodingError Header) -> m ())
  -> m ()
processSerialPort handler =
  runEffect . runSafeP $ do
    serial <- liftIO openMySerial
    forever $ PBS.hGetSome 1 serial
    >-> PP.parseForever msgParser
    >-> P.mapM_ (lift . handler)
