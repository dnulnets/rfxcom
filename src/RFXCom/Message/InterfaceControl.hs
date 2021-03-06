{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- |This is the RFXCom Message base file that contains the RFXCom message data structure
-- for the interface control message.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.InterfaceControl (
  Body(..),
  Command(..)
  ) where

--
-- Import section
--

import           Control.Monad              (replicateM_)

import           Data.Binary                (Word8)
import           Data.Binary.Put            (putWord8)
import           Data.Bits

--
-- Internal Import Section
--
import qualified RFXCom.Message.BaseMessage as BM (Header (..),
                                                   RFXComMessage (..))

-- |The command that is sent on the message
data Command = Reset     -- ^Reset the RFXCom Device
             | GetStatus -- ^Get the status of the RFXCom device
             | Start     -- ^Start the reception of the RFXCom device
             deriving (Show)

-- |The body of the interface control message
data Body = Body
  {
    _cmnd::Command -- ^The command to send as specified by 'Command'
  }
  deriving (Show)

-- |Instance definition of the temperature and humidity sensor reading message
instance BM.RFXComMessage Body where

  -- |This message cannot ever be sent from the RFXCom device so we do not need to be able
  -- to parse it.
  getMessage = undefined

  -- |This message is written to the RFXCom device
  putMessage seqnr item = do
    putWord8 13
    putWord8 0
    putWord8 0
    putWord8 seqnr
    case _cmnd item of
      Reset ->
        putWord8 0
      GetStatus ->
        putWord8 2
      Start ->
        putWord8 7
    replicateM_ 9 $ putWord8 0

