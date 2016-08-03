{-# OPTIONS_HADDOCK ignore-exports #-}
-- |This module contains all different RFXCom 'Message' types and constructors.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.Base (
  Message(..)
  ) where

--
-- External Import section
--

--
-- Internal Import Section
--
import           RFXCom.Message.BaseMessage            (Header, RawBody)
import           RFXCom.Message.InterfaceControl       (InterfaceControlBody)
import           RFXCom.Message.InterfaceResponse       (InterfaceResponseBody)
import           RFXCom.Message.Security1              (Security1Body)
import           RFXCom.Message.TemperatureAndHumidity (TemperatureAndHumidityBody)

-- |This data structure holds all of the possible messages to and from the RFXCom
-- device. To extend this structure just add a typeconstructor and create or
-- reuse an existing message type.
--
-- Make sure you add the appropriate type classes (RFXComMessage) to the message
-- type.
--
-- In this file:
--
-- data Message = .....
--              | <New message constructor> <new message body type>
--
-- In <message boy type> hs-file:
--
-- instance RFXComMessage  <new Message body type> where
--   ....
--
-- In Decode file:
--
-- Add pattern matching to msgDecoderMux for the message type (_type field)
-- and create the appropriate message body and use the new message
-- constructor
--
-- In Encode file:
--
-- Add pattern matching to msgEncoderMux for the message constructor and
-- call putMessage for the body
--

-- |This type contains all of the messages sent to and from the RFXCom device.
data Message
  -- |This is a corrupted message, i.e. something in the byte stream is wrong. Usually it
  -- is the length of the message that is wrong or something in the message is not handled
  -- or otherwise corrupt. The String contains additional information.
  = CorruptMessage String

  -- |This is an unknown message, i.e. a message that has the correct header but the type
  -- is unknown or not handled. The information contains the raw binary body.
  | UnknownMessage RawBody

  -- |This is the temperature and humidity sensor reading message.
  | TemperatureAndHumidity TemperatureAndHumidityBody

  -- |This is the security 1 sensor reading message
  | Security1 Security1Body

  -- |This is the interface control message
  | InterfaceControl InterfaceControlBody

  -- |This is the response to the interface control message
  | InterfaceResponse InterfaceResponseBody
  deriving (Show)

