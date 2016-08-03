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
  InterfaceResponseBody(..),
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
import qualified RFXCom.Message.BaseMessage as H (Header(..),
                                                  RFXComMessage(..))

-- |The interface control response message
data InterfaceResponseBody = InterfaceResponseBody
  {
    _subtype::Word8
  , _cmnd::Word8
  , _data::[Word8]
  }
  deriving (Show)


-- |Instance definition of the interface response message
instance H.RFXComMessage InterfaceResponseBody where
  
  -- |The message parser for the 'InterfaceResponseBody' structure
  getMessage header =
    if ((H._size header == 13) && (H._subtype header /= 7)) ||
       ((H._size header == 14) && (H._subtype header == 7)) then do
      
      cmnd <- getWord8
      arr <- unpack <$> (getByteString $ fromIntegral (H._size header) - 5)
      return $ Right $ InterfaceResponseBody (H._subtype header) cmnd arr
    
    else do
    
      return $ Left "Wrong size of the interface response message, it must be ten or fourteen bytes depending on subtype"

  -- |This message cannot be sent
  putMessage = undefined
