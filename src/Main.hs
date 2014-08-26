module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Error

-- happstack package
import Data.Acid
import Happstack.Server
import Happstack.Server.ClientSession

-- html pages
import Html.Error
import Html.Index

import Session
import State
import Routes

main :: IO ()
main = do

  key  <- getDefaultKey
  acid <- openLocalState emptyBlogState

  simpleHTTP hsconf $
    runServerT acid (sessionconf key) $
      mainRoute `catchError` internalServerErrorResponse

 where

  -- happstack config
  hsconf = nullConf { port = 8686 }

  -- session config
  sessionconf key = (mkSessionConf key)
    { sessionCookieLife = oneMonth
    , sessionCookieName = "blog.user.session"
    }

  oneMonth        = MaxAge $ 60 * 60 * 24 * 7 * 4

mainRoute :: ServerT Response
mainRoute = msum
  [ dir "static" $ serveDirectory DisableBrowsing ["index.html"] "static"
  , dir "api"    $ apiRoute
  , pageRoute
  , notFoundResponse
  ]

--
-- Error responses
--

internalServerErrorResponse :: IOException -> ServerT Response
internalServerErrorResponse =
  internalServerError . toResponse . page500InternalError

notFoundResponse :: ServerT Response
notFoundResponse = notFound $ toResponse $ page404NotFound
