-- |This is the RFXCom Message base file that contains the RFXCom message data structures.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.BaseMessage (
  Header(..),
  RawMessageBody(..),
  RFXComMessage(..)
  ) where

--
-- Import section
--
import           Data.Binary     (Word8)
import           Data.Binary.Get (Get, getWord8)

import           Control.Monad   (replicateM)

--
-- Internal Import Section
--

-- |The class for handling all different types of RFX messages.
class RFXComMessage a where

  -- |This is the RFX message body parser using the 'Get' monad. It do not parses the header
  -- that it assumes is already done and also passed in as an argument.
  getMessage::Header -- ^The header of the message
            ->Get a -- ^The binary message parser

-- |The RFXCom message header that is the first sequence of bytes in every message.
data Header = Header
  { _size :: !Word8 -- ^The size of the message
  , _type :: !Word8 -- ^The type of the message
  , _subtype :: !Word8 -- ^The subtype of the message
  , _sequenceNumber :: !Word8} -- ^The sequence number of the message
  deriving (Show)

-- |The generic message type when the message is unknown, not handled by this library
-- or no specific message type is needed. It just contains the raw message body.
data RawMessageBody = RawMessageBody
  { _data           :: ![Word8]} -- ^The raw body of the message
  deriving (Show)

-- |Clamp the body size of the message, it must be more than 3 bytes which is the header
-- size. If the size is less than 3 we have an invalida message.
clampBodySize::Word8 -- ^The size of the message (from the headers first byte)
             ->Word8 -- ^The size of the body of the message (after removing the header size)
clampBodySize x | x>3 = x-3
                | otherwise = 0

-- |Instance definition of the base message
instance RFXComMessage RawMessageBody where
  
  -- |The message parser for the 'RawMessageBody'.
  getMessage header = do
    mdata <- replicateM (fromIntegral $ clampBodySize (_size header)) getWord8
    return $! RawMessageBody mdata
