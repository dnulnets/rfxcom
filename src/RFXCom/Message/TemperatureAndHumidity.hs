{-# OPTIONS_HADDOCK ignore-exports #-}
-- |This is the RFXCom Message base file that contains the RFXCom message data structure
-- for the temperature and humidity sensor readings.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Message.TemperatureAndHumidity (
  TemperatureAndHumidityBody(..),
  HumidityStatus (..)
  ) where

--
-- Import section
--
import           Data.Bits
import           Data.Binary     (Word8)
import           Data.Binary.Get (Get, getWord8, getWord16be)

import           Control.Monad   (replicateM)

--
-- Internal Import Section
--
import RFXCom.Message.BaseMessage (Header(..),
                                   RFXComMessage(..))

-- |The interpretation of the humidity value.
data HumidityStatus = Normal
                    | Comfort
                    | Dry
                    | Wet
                    | Unknown
                    deriving (Show)

-- |The temperature and humidity sensor reading message.
data TemperatureAndHumidityBody = TemperatureAndHumidityBody
  {_id::Integer                    -- ^The identity of the sensor
  ,_temperature::Float             -- ^The temperature reading of the sensor
  ,_humidity::Float                -- ^The humidity of the sensor [0-100%] RH
  ,_humidityStatus::HumidityStatus -- ^The interpretation of the humidity value
  ,_batteryLevel::Integer          -- ^The battery level of the sensor [0(empty)-9(full)]
  ,_rssi::Integer}                 -- ^The signal strength of the sensor reception [0(weak)-15(strong)]
  deriving (Show)

temperature::Word8->Word8->Float
temperature h l = sgn * (hv * 256.0 + lv)/10.0
  where
    hv = fromIntegral (h .&. 0x7f)
    lv = fromIntegral l
    sgn = 1.0 - 2.0 * fromIntegral (h .&. 0x80)

humidityStatus::Word8->HumidityStatus
humidityStatus 0 = Normal
humidityStatus 1 = Comfort
humidityStatus 2 = Dry
humidityStatus 3 = Wet
humidityStatus _ = Unknown

batteryLevel::Word8->Integer
batteryLevel sts =  10*(1+fromIntegral (shiftR (sts .&. 0xf0) 4))

rssiLevel::Word8->Integer
rssiLevel sts = fromIntegral (sts .&. 0x0f)

-- |Instance definition of the temperature and humidity sensor reading message
instance RFXComMessage TemperatureAndHumidityBody where
  
  -- |The message parser for the 'TemperatureAndHumidityBody'.
  getMessage header = do
    mid <- Right $ fromIntegral <$> getWord16be
    mtemperatureH <- Right getWord8
    mtemperatureL <- Right getWord8
    mhumidity <- Right $ fromIntegral <$> getWord8
    mhumidityStatus <- Right $ humidityStatus <$> getWord8
    msensorStatus <- Right getWord8
    return $! TemperatureAndHumidityBody <$> mid <*> (fromIntegral <$> mtemperatureH) <*> mhumidity <*> mhumidityStatus <*> (fromIntegral <$> msensorStatus) <*> (fromIntegral <$> msensorStatus)


