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
import RFXCom.Message.BaseMessage (Header,
                                   RawMessageBody)

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
--              | <New message constructor> <new message type>
--
-- In <message type> file:
--
-- instance RFXComMessage  <new Message type> where
--   ....
--
data Message
  -- |This is a corrupted message, i.e. something in the byte stream is wrong. Usually it
  -- is the length of the message that is wrong.
  = CorruptMessage
  
  -- |This is an unknown message, i.e. a message that has the correct header but the type
  -- is unknown. The message contains the raw data bytes.
  | UnknownMessage Header RawMessageBody
  
  deriving (Show)
