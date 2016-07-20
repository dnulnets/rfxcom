{-# OPTIONS_HADDOCK ignore-exports #-}
-- |This is the RFXCom Message base file that contains the RFXCom message data structure
-- for the security 1 sensor readings.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.Security1 (
  Security1Body(..),
  ) where

--
-- Import section
--
import           Data.Binary                (Word8)
import           Data.Binary.Get            (Get, getByteString, getWord8)
import           Data.ByteString            (unpack)

import           Control.Monad              (replicateM)

--
-- Internal Import Section
--
import           RFXCom.Message.BaseMessage (Header (..), RFXComMessage (..))

-- |The temperature and humidity sensor reading message.
data Security1Body = Security1Body
  { _data :: ![Word8]} -- ^The raw body of the message
  deriving (Show)

-- |Clamp the body size of the message, it must be more than 3 bytes which is the header
-- size. If the size is less than 3 we have an invalida message.
clampBodySize::Word8 -- ^The size of the message (from the headers first byte)
             ->Word8 -- ^The size of the body of the message (after removing the header size)
clampBodySize x | x>3 = x-3
                | otherwise = 0

-- |Instance definition of the temperature and humidity sensor reading message
instance RFXComMessage Security1Body where

  -- |The message parser for the 'Security1Body'
  getMessage hdr = do
    if size>0 then
      (Right . Security1Body . unpack )  <$> (getByteString $ size)
      else
      return $ Left "Wrong size of the message body, it must be at least one byte"
      where
        size = fromIntegral (_size hdr) - 3
