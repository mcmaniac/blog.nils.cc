{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module State.Users where

import Control.Lens
import Control.Monad.Trans

import Crypto.Scrypt

import Data.Data
import Data.Maybe
import Data.ByteString (ByteString)
import Data.IxSet
import Data.Text (Text)
import Data.SafeCopy
import Data.Unique

import State.Helper

--
-- User
--

type UserID = Int

data UserRole
  = Admin
  | Author
  | Subscriber
  deriving (Eq, Ord, Typeable, Data)

deriveSafeCopy 0 'base ''UserRole

-- The User type with all newtype wrappers

data User = User
  { _userID         :: UserID
  , _userName       :: Text
  , _userPwd        :: ByteString
  , _userRole       :: UserRole
  }
  deriving (Eq, Ord, Typeable, Data)

makeLenses ''User

deriveSafeCopy 0 'base ''User

--
-- lenses to unwrap newtypes
--

-- | Custom lens for encrypted passwords
userPassword :: Lens' User EncryptedPass
userPassword = userPwd . encrypted

-- | Lens for converting bytestring to scrypt's `EncryptedPass`
encrypted :: Lens' ByteString EncryptedPass
encrypted = lens EncryptedPass (const getEncryptedPass)

--
-- Lookup map
--

inferIxSet "UserDB" ''User 'noCalcs
  [ ''UserID
  , ''Text
  , ''UserRole
  ]

--
-- Lenses
--

userAt :: UserID -> Lens' UserDB (Maybe User)
userAt = ixSetAt

role :: UserRole -> Getter UserDB UserDB
role r = to $ (@= r)

admins :: Getter UserDB UserDB
admins = role Admin

authors :: Getter UserDB UserDB
authors = role Author

subscriber :: Getter UserDB UserDB
subscriber = role Subscriber

--
-- UserIDs
--

newUniqueUserId :: MonadIO m => Action m UserDB UserID
newUniqueUserId = act pickId
 where
  pickId db = do
    unique <- liftIO newUnique
    let i = hashUnique unique
    if isJust $ db ^. userAt i
      then pickId db
      else return i
