{-# LANGUAGE TemplateHaskell #-}

module Session where

import Control.Applicative
import Control.Monad.Reader

-- lens
import Control.Lens

-- happstack framework
import Data.Acid
import Data.SafeCopy
import Happstack.Server
import Happstack.Server.ClientSession

import Text.I18n

-- local
import State
import State.Helper
import State.Users

type ServerT a = ClientSessionT SessionData AcidServerT a

runServerT
  :: AcidState BlogState
  -> SessionConf
  -> ServerT a
  -> ServerPart a
runServerT acid sconf srvt =
  runReaderT (withClientSessionT sconf srvt) acid

--
-- Type definition
--

data SessionData = SessionData
  { _sessionUser    :: Maybe UserID
  , _sessionLocale  :: Maybe String
  }

makeLenses ''SessionData

deriveSafeCopy 0 'base ''SessionData

instance ClientSession SessionData where
  emptySession = SessionData
    { _sessionUser   = Nothing
    , _sessionLocale = Nothing
    }

--
-- Requests
--

getUserID :: ServerT (Maybe UserID)
getUserID = liftSessionStateT $ use sessionUser

setUserID :: Maybe UserID -> ServerT ()
setUserID mid = liftSessionStateT $ sessionUser .= mid

getSessionUser :: ServerT (Maybe User)
getSessionUser = do
  muid  <- getUserID
  maybe (return Nothing) (runQuery . GetUserById) muid

getSessionLocale :: ServerT (Maybe Locale)
getSessionLocale = liftSessionStateT $
  fmap Locale <$> use sessionLocale

setSessionLocale :: Maybe Locale -> ServerT ()
setSessionLocale mloc = liftSessionStateT $
  sessionLocale .= (unloc <$> mloc)
 where
  unloc (Locale s) = s
