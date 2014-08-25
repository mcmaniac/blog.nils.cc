{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module State.Users where

import Control.Lens

import Crypto.Scrypt

import Data.Data
import Data.ByteString (ByteString)
import Data.IxSet
import Data.Text (Text)
import Data.SafeCopy

import State.Helper

--
-- User
--

type UserID = Int

-- The User type with all newtype wrappers

data User = User
  { _userID        :: UserID
  , _userName      :: Text
  , _userPwd       :: ByteString
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
  ]

--
-- Lenses
--

userAt :: UserID -> Lens' UserDB (Maybe User)
userAt = ixSetAt
