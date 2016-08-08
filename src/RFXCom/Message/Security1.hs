{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- |This is the RFXCom Message base file that contains the RFXCom message data structure
-- for the security 1 sensor readings.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.Security1 (
  Body(..)
  ) where

--
-- Import section
--
import           Data.Binary                (Word8)
import           Data.Binary.Get            (getByteString, getWord8)
import           Data.ByteString            (unpack)

import           Control.Monad              (replicateM)

--
-- Internal Import Section
--
import qualified RFXCom.Message.BaseMessage as BM (Header (..),
                                                   RFXComMessage (..))

-- |The security 1 message body
data Body = Body
  { _data :: ![Word8]} -- ^The raw body of the message
  deriving (Show)

-- |Instance definition of the temperature and humidity sensor reading message
instance BM.RFXComMessage Body where

  -- |The message parser for the 'Security1Body'
  getMessage hdr = do
    if size>0 then
      (Right . Body . unpack )  <$> (getByteString $ size)
      else
      return $ Left "Wrong size of the message body, it must be at least one byte"
      where
        size = fromIntegral (BM._size hdr) - 3

  -- |This message cannot be sent
  putMessage = undefined
