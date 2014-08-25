{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module State.Posts where

import Control.Lens

import Data.Data (Data, Typeable)
import Data.Time
import Data.Text (Text)
import Data.SafeCopy
import Data.IxSet as IxS

-- my modules
import State.Helper

--
-- Post
--

data Content
  = FileContent   { _fileContent  :: FilePath }
  | ImageContent  { _imageContent :: FilePath }
  | TextContent   { _textContent  :: Text     }
  deriving (Eq, Ord, Typeable, Data, Show)

makeLenses ''Content

deriveSafeCopy 0 'base ''Content

data Visibility
  = Public
  | Private
  | Trashed
  deriving (Eq, Ord, Typeable, Data, Show)

deriveSafeCopy 0 'base ''Visibility

data Post = Post
  { _postTitle          :: Text
  , _postPublishTime    :: UTCTime      -- ^ indexed
  , _postFilePath       :: FilePath     -- ^ indexed
  , _postContent        :: [Content]
  , _postVisibility     :: Visibility
  }
  deriving (Eq, Ord, Typeable, Data, Show)

makeLenses ''Post

deriveSafeCopy 0 'base ''Post

--
-- Lookup map
--

inferIxSet "PostDB" ''Post 'noCalcs
  [ ''UTCTime
  , ''FilePath
  ]

--
-- Lenses
--

recentPosts
  :: Int                      -- ^ Offset
  -> Int                      -- ^ Limit
  -> Getter PostDB [Post]
recentPosts offset limit = to $
  take limit . drop offset . toDescList (Proxy :: Proxy UTCTime)

postAt :: FilePath -> Lens' PostDB (Maybe Post)
postAt = ixSetAt
