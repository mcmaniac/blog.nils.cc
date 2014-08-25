{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module State.Config where

import Control.Lens

import Data.SafeCopy
import Data.Data

-- local modules
import State.Posts

--
-- Config type
--

data Config = Config
  { _configDefaultVisibility    :: Visibility
  }
  deriving (Eq, Ord, Typeable, Data, Show)

makeLenses ''Config

deriveSafeCopy 0 'base ''Config
