{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module State.Users where

import Data.Typeable
import Data.IntMap
import Data.ByteString (ByteString)

-- lens
import Control.Lens

-- text
import Data.Text (Text)

-- safecopy
import Data.SafeCopy

-- scrypt
import Crypto.Scrypt

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
  deriving (Eq, Ord, Typeable)

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

newtype UserDB = UserDB { _userDB :: IntMap User }

makeLenses ''UserDB

deriveSafeCopy 0 'base ''UserDB
