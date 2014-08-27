{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module State.Config where

import Control.Lens

import Data.SafeCopy
import Data.Data

import Text.I18n

-- local modules
import State.Helper
import State.Posts

--
-- Config type
--

data Config = Config
  { _configDefaultVisibility    :: Visibility
  , _configDefaultLocaleStr     :: String
  }
  deriving (Eq, Ord, Typeable, Data, Show)

makeLenses ''Config

deriveSafeCopy 0 'base ''Config

defaultConfig :: Config
defaultConfig = Config
  { _configDefaultVisibility = Private
  , _configDefaultLocaleStr  = "en"
  }

configDefaultLocale :: Lens' Config Locale
configDefaultLocale = configDefaultLocaleStr . locale
