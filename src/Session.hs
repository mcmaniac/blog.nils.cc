{-# LANGUAGE TemplateHaskell #-}

module Session where

import Control.Monad.Reader

-- lens
import Control.Lens

-- happstack framework
import Data.Acid
import Data.SafeCopy
import Happstack.Server
import Happstack.Server.ClientSession

-- local
import State
import State.Users

type ServerT a = ReaderT (AcidState BlogState) (ClientSessionT SessionData (ServerPartT IO)) a

runServerT
  :: (Key -> SessionConf)
  -> ServerT a
  -> ServerPart a
runServerT sconf srvt = do
  key  <- liftIO $ getDefaultKey
  acid <- liftIO $ openLocalState emptyBlogState
  runServerT' acid (sconf key) srvt

runServerT'
  :: AcidState BlogState
  -> SessionConf
  -> ServerT a
  -> ServerPart a
runServerT' acid sconf srvt =
  withClientSessionT sconf $ runReaderT srvt acid

--
-- Type definition
--

data SessionData = SessionData
  { _sessionUser  :: Maybe UserID
  }

makeLenses ''SessionData

deriveSafeCopy 0 'base ''SessionData

instance ClientSession SessionData where
  emptySession = SessionData { _sessionUser = Nothing }

--
-- Requests
--

getUserID :: ServerT (Maybe UserID)
getUserID = liftSessionStateT $ use sessionUser

setUserID :: Maybe UserID -> ServerT ()
setUserID mid = liftSessionStateT $ sessionUser .= mid
