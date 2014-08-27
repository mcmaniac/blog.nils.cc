{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module State where

import Control.Monad.Reader
import Data.Data

-- lens
import Control.Lens

-- happstack framework
import Data.Acid
import Data.SafeCopy
import Data.IxSet (empty)
import Happstack.Server

import Text.I18n

-- local modules
import State.Users
import State.Posts
import State.Config

data BlogState = BlogState
  { _users    :: UserDB
  , _posts    :: PostDB
  , _config   :: Maybe Config
  }
  deriving (Eq, Ord, Typeable, Data)

makeLenses ''BlogState

deriveSafeCopy 0 'base ''BlogState

emptyBlogState :: BlogState
emptyBlogState = BlogState
  { _users = empty
  , _posts = empty
  , _config = Nothing
  }

--
-- Queries
--

getUserById :: UserID -> Query BlogState (Maybe User)
getUserById uid = view $ users . userAt uid

getConfig :: Query BlogState (Maybe Config)
getConfig = view config

getDefaultLocale :: Query BlogState (Maybe String)
getDefaultLocale = asks (^? config . _Just . configDefaultLocaleStr)

--
-- Update
--

setConfig :: Maybe Config -> Update BlogState ()
setConfig c = config .= c

setDefaultVisibility :: Visibility -> Update BlogState ()
setDefaultVisibility v =
  config . non defaultConfig . configDefaultVisibility .= v

--
-- Acid instance
--

makeAcidic ''BlogState
  [ -- queries
    'getUserById
  , 'getConfig
  , 'getDefaultLocale
    -- updates
  , 'setConfig
  , 'setDefaultVisibility
  ]

--
-- Acidic server functions
--

runQuery
  :: (QueryEvent event, EventState event ~ BlogState,
      MonadIO m, MonadReader (AcidState BlogState) m)
  => event -> m (EventResult event)
runQuery event = do
  acid <- ask
  liftIO $ query acid event

runUpdate
  :: (UpdateEvent event, EventState event ~ BlogState,
      MonadIO m, MonadReader (AcidState BlogState) m)
  => event -> m (EventResult event)
runUpdate event = do
  acid <- ask
  liftIO $ update acid event

type AcidServerT = ReaderT (AcidState BlogState) (ServerPartT IO)
