{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This application converts rfxcom messages to MQTT messages on a message bus
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.Encoder ( msgEncoder ) where

--
-- External Import section
--
import           Data.Binary
import           Data.Binary.Put
import           Data.ByteString            (ByteString, concat)
import qualified Data.ByteString.Lazy       as BL (toChunks)
import           Data.Word                  (Word16, Word8)

--
-- Internal import section
--
import           RFXCom.Message.Base        (Message (..))
import           RFXCom.Message.BaseMessage (Header (..), RFXComMessage,
                                             putMessage)

-- |The message enacoder
msgEncoder::Word8      -- ^The sequence number
          ->Message    -- ^The message
          ->ByteString -- ^The sequence of bytes that the message is covnerted to
msgEncoder seqNr msg = Data.ByteString.concat $ BL.toChunks $ runPut $ msgEncoderMux seqNr msg

-- |The message encoder mux that uses the bodys putMessage
msgEncoderMux::Word8   -- ^The sequence number
             ->Message -- ^The message
             ->Put     -- ^The Put monad containig the sequence of bytes
msgEncoderMux seqNr all@(InterfaceControl body) = putMessage seqNr body
