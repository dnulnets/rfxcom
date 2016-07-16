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
                                                   encode)
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
  { _size           :: Word8
  , _type           :: Word8
  , _subtype        :: Word8
  , _sequenceNumber :: Word8
  , _data           :: [Word8]}
  deriving (Show)

--
-- Main to test the pipe sequence
--
main :: IO ()
main = processSerialPort
     $ liftIO . print

--
-- Cuts a binary string into several bytes
--
binaryStringToBytes::(Monad m) => Pipe ByteString Word8 m r
binaryStringToBytes = P.mapFoldable unpack

testit::(Monad m) => PP.Parser ByteString m ([Either PB.DecodingError Word8])
testit = sequence [PB.decode,PB.decode,PB.decode]

testita::(Monad m) => PP.Parser ByteString m (Either PB.DecodingError [Word8])
testita = mapM id <$> sequence [PB.decode,PB.decode,PB.decode]

testitb::(Monad m) => PP.Parser ByteString m (Either PB.DecodingError [Word8])
testitb = sequence <$> sequence [PB.decode,PB.decode,PB.decode]

--
-- Decode a stream of binaries to a generic Header, in case of decoding errors
-- just return Nothing.
--
decodeMessage::(Monad m) => PP.Parser ByteString m (Either PB.DecodingError Header)
decodeMessage = do
  msize <- PB.decode
  mtype <- PB.decode
  msubtype <- PB.decode
  msequenceNumber <- PB.decode
  mdata <- sequence <$> replicateM 3 PB.decode
  return $ (Header <$> msize <*> mtype <*> msubtype <*> msequenceNumber <*> mdata)

--
-- Decode a stream of binaries to a generic Header, in case of decoding errors
-- just return Nothing.
--
validateBodySize::Word8->Int
validateBodySize x | x>3 = fromIntegral x-3
                 | otherwise = 0

validateData::(Either PB.DecodingError [Word8])->(Either PB.DecodingError [Word8])
validateData (Right ls) = if null ls then
                  Left $ PB.DecodingError 0 "Empty Message"
                else
                  Right ls
validateData (Left de) = Left de


decodeMessage2::(Monad m) => PP.Parser ByteString m (Either () (Either PB.DecodingError Header))
decodeMessage2 = do
  msize <- PB.decode
  mtype <- PB.decode
  msubtype <- PB.decode
  msequenceNumber <- PB.decode
  mdata <- sequence <$> replicateM (either (const 0) id (validateBodySize <$> msize)) PB.decode
  return $ Right $ (Header <$> msize <*> mtype <*> msubtype <*> msequenceNumber <*> validateData mdata)

openMySerial :: IO Handle
openMySerial = hOpenSerial "/dev/ttyUSB0" defaultSerialSettings { commSpeed = CS38400,
                                                                  bitsPerWord = 8,
                                                                  stopb = One,
                                                                  parity = NoParity,
                                                                  timeout = 10}

--
-- process Serial
--
processSerialPort ::
     (MonadIO m, MonadMask m)
  => ((Either PB.DecodingError Header) -> m ())
  -> m ()
processSerialPort handler =
  runEffect . runSafeP $ do
    serial <- liftIO openMySerial
    forever $ PBS.hGetSome 1 serial
    -- forever $ PBS.hGetSome 256 serial

    -- >-> P.mapM (lift . liftIO . print)
    
    -- Use this to test this more simply, otherwise it would be the serial handle
    --PBS.fromLazy $ fromStrict $ pack [4,2, 1,64::Word8, 2,3,2,65,2,5,5,1,2,3,5]

    >-> (PP.parseForever decodeMessage2)

    >-> P.mapM_ (lift . handler)
