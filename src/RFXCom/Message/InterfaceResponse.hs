{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- |This is the RFXCom Message base file that contains the RFXCom message data structure
-- for the interface response messages
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.InterfaceResponse (
  Body(..),
  ) where

--
-- Import section
--

import           Data.Bits
import           Data.Binary     (Word8)
import           Data.Binary.Get (getWord8,getByteString)

import           Data.ByteString (unpack)

--
-- Internal Import Section
--
import qualified RFXCom.Message.BaseMessage as BM (Header(..),
                                                  RFXComMessage(..))

-- |The interface control response message body
data Body = Body
  {
    _subtype::Word8
  , _cmnd::Word8
  , _data::[Word8]
  }
  deriving (Show)


-- |Instance definition of the interface response message
instance BM.RFXComMessage Body where
  
  -- |The message parser for the 'InterfaceResponseBody' structure
  getMessage header =
    if ((BM._size header == 13) && (BM._subtype header /= 7)) ||
       ((BM._size header == 20) && (BM._subtype header == 7)) then do
      
      cmnd <- getWord8
      arr <- unpack <$> (getByteString $ fromIntegral (BM._size header) - 4)
      return $ Right $ Body (BM._subtype header) cmnd arr
    
    else do
    
      return $ Left "Wrong size of the interface response message, it must be thirteen or twenty bytes depending on subtype"

  -- |This message cannot be sent
  putMessage = undefined
