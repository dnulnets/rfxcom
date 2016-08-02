{-# OPTIONS_HADDOCK ignore-exports #-}
-- |This application converts rfxcom messages to MQTT messages on a message bus
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.Encoder ( msgEncoder ) where

--
-- External Import section
--
import           Data.Binary
import           Data.Binary.Put
import           Data.ByteString            (ByteString, concat)
import qualified Data.ByteString.Lazy       as BL (toChunks)
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
import           RFXCom.Message.BaseMessage (Header (..), RFXComMessage,
                                             putMessage)


msgEncoder::Word8->Message->ByteString
msgEncoder seqNr msg = Data.ByteString.concat $ BL.toChunks $ runPut $ msgEncoderMux seqNr msg

msgEncoderMux::Word8->Message->Put
msgEncoderMux seqNr all@(InterfaceControl body) = putMessage seqNr body
