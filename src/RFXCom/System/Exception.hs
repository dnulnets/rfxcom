-- |This file contains the exception definition used in this library
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.System.Exception (
  ResourceException(..)
  ) where

--
-- Import section
--
import           Control.Exception

--
-- Internal import section
--

-- |The resource exception used in this application.
data ResourceException = ResourceException String
  deriving (Show)

-- |Must be an instance of the exception class.
instance Exception ResourceException
