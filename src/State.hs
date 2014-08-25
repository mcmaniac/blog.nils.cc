{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module State where

import Data.Data

import Control.Lens

-- happstack framework
import Data.Acid
import Data.SafeCopy
import Data.IxSet (empty)

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


--
-- Acid instance
--

makeAcidic ''BlogState
  [
  ]
