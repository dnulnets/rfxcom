{-# OPTIONS_HADDOCK ignore-exports #-}
-- |This application converts rfxcom messages to MQTT messages on a message bus
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.Decoder (
  msgParser ) where

--
-- External Import section
--
import           Data.Binary
import           Data.Binary.Get
import           Data.ByteString            (ByteString)
import           Data.Word                  (Word16, Word8)

import           System.Hardware.Serialport (CommSpeed (..),
                                             SerialPortSettings (..),
                                             defaultSerialSettings, hOpenSerial)

import           Control.Applicative
import           Control.Monad

import qualified Pipes.Binary               as PB (DecodingError (..),
                                                   decodeGet)
import qualified Pipes.Parse                as PP

--
-- Internal import section
--
import           RFXCom.Message.Base        (Message (..))
import           RFXCom.Message.BaseMessage (Header(..),
                                             RFXComMessage,
                                             getMessage)

-- |Parses the bytestream into rfxcom messages based on the rfxcom decoder and return with
-- decoding errors or the rfxcom message.
msgParser::(Monad m) => PP.Parser ByteString m (Either () (Either PB.DecodingError Message)) -- ^The parser for the RFXCom message
msgParser = do
  msg <- PB.decodeGet msgDecoder
  return $ Right msg

-- |Parses the bytestream using the 'Get' monad to a Message.
msgDecoder::Get Message -- ^The decoder containig the message
msgDecoder = do
    msize <- getWord8
    if msize > 3
      then do
        mtype <- getWord8
        msubtype <- getWord8
        msqnr <- getWord8
        either ( return . CorruptMessage) id $ msgDecoderMux $ Header msize mtype msubtype msqnr
      else
        return $ CorruptMessage "Size byte of the message is not corrent, it must have a value greater than 3. Byte is ignored."

-- |Handles the conversion from the RFXCom message type value to an actual message type constructor
msgDecoderMux::Header                      -- ^The header of the message
             ->Either String (Get Message) -- ^An error string or the decoder containing the message
msgDecoderMux hdr@(Header _ 0x20 _ _) = (Security1 hdr <$>) <$> getMessage hdr
msgDecoderMux hdr@(Header _ 0x52 _ _) = (TemperatureAndHumidity hdr <$>) <$> getMessage hdr
msgDecoderMux hdr = (UnknownMessage hdr <$>) <$> getMessage hdr
