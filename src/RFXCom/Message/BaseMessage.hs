{-# OPTIONS_HADDOCK ignore-exports #-}
-- |This is the RFXCom Message base file that contains the RFXCom message data structures.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.BaseMessage (
  Header(..),
  RawBody(..),
  RFXComMessage(..),
  validateAndCalculateBodySize
  ) where

--
-- Import section
--
import           Data.Binary     (Word8)
import           Data.Binary.Get (Get, getByteString, getWord8)

import           Data.ByteString (unpack)

import           Control.Monad   (replicateM)

--
-- Internal Import Section
--

-- |The class for handling all different types of RFX messages.
class RFXComMessage a where

  -- |This is the RFX message body parser using the 'Get' monad. It do not parses the header
  -- that it assumes is already done and also passed in as an argument.
  getMessage::Header                -- ^The header of the message
            ->Get (Either String a) -- ^The binary message parser

-- |The RFXCom message header that is the first sequence of bytes in every message.
data Header = Header
  { _size           :: !Word8 -- ^The size of the message
  , _type           :: !Word8 -- ^The type of the message
  , _subtype        :: !Word8 -- ^The subtype of the message
  , _sequenceNumber :: !Word8} -- ^The sequence number of the message
  deriving (Show)

--
-- Set of functions that helps in parsing the messages
--

-- |Validate and calculate the body size of the message, i.e. the chunk of data that comes
-- after the Header. The total message must be more than 3 bytes which is the header
-- size, i.e. every message must have at least one byte in the body.
--
-- If the size is less than 3 we have an invalida message.
validateAndCalculateBodySize::Word8             -- ^The size of the message (from the headers first byte)
                            ->Either String Int -- ^The size of the body of the message (after removing the header size)
validateAndCalculateBodySize x | x>3 = Right $ fromIntegral x-3
                               | otherwise = Left "The length of the message is invalid, it must be longer than three bytes."

--
-- The base message (raw data message) functionality
--

-- |The generic message type when the message is unknown, not handled by this library
-- or no specific message type is needed. It just contains the raw message body.
data RawBody = RawBody
  { _data :: ![Word8]} -- ^The raw body of the message
  deriving (Show)

-- |Instance definition of the raw body  message
instance RFXComMessage RawBody where

  -- |The message parser for the 'RawBody'.
  getMessage hdr = do
    if size>0 then
      (Right . RawBody . unpack )  <$> (getByteString $ size)
      else
      return $ Left "Wrong size of the message body, it must be at least one byte"
      where
        size = fromIntegral (_size hdr) - 3

