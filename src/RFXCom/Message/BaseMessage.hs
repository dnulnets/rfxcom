{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- |This is the RFXCom Message base file that contains the RFXCom message data structures.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.BaseMessage (
  Header(..),
  Body(..),
  RFXComMessage(..)
  ) where

--
-- Import section
--
import           Data.Binary     (Word8)
import           Data.Binary.Get (Get, getByteString)
import           Data.Binary.Put (Put)
import           Data.ByteString (unpack)

import           Control.Monad   (replicateM)

--
-- Internal Import Section
--

-- |The class for handling all different types of RFX messages.
class RFXComMessage a where

  -- |This is the RFX message body parser using the 'Get' monad. It do not parses the header
  -- because we need to know what kind of message this is before we can parse it.
  getMessage::Header                -- ^The header of the message, already parsed
            ->Get (Either String a) -- ^The binary message parser that returns a string error or the message body

  -- |This is the RFX Message body producer using the 'Put' monad. It converts the message
  -- to the appropriate byte message
  putMessage::Word8 -- ^The sequence number of the message
            ->a     -- ^The message
            ->Put   -- ^The binary sequence of the message, including the header

-- |The RFXCom message header that is the first sequence of bytes in every message.
data Header = Header
  { _size           :: !Word8  -- ^The size of the message
  , _type           :: !Word8  -- ^The type of the message
  , _subtype        :: !Word8  -- ^The subtype of the message
  , _sequenceNumber :: !Word8} -- ^The sequence number of the message
  deriving (Show)


--
-- The base message (raw data message) functionality
--


-- |The generic message type when the message is unknown, not handled by this library
-- or no specific message type is needed. It just contains the raw message body.
data Body = Body
  { _data :: ![Word8]} -- ^The raw body of the message
  deriving (Show)

-- |Instance definition of the raw body  message
instance RFXComMessage Body where

  -- |The message parser for the 'RawBody'.
  getMessage hdr = do
    if size>0 then
      (Right . Body . unpack )  <$> (getByteString $ size)
      else
      return $ Left "The message is corrupt, the length of the message must be longer than four."
      where
        size = fromIntegral (_size hdr) - 3

  -- |We cannot send raw data bodies
  putMessage = undefined
