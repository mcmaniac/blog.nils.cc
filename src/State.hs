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

--
-- Acid instance
--

makeAcidic ''BlogState
  [ 'getUserById
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
