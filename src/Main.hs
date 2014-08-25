module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Error

-- happstack package
import Happstack.Server
import Happstack.Server.ClientSession

-- html pages
import Html.Error
import Html.Index

import Session
import State

main :: IO ()
main = do

  simpleHTTP hsconf $ runServerT sessionconf $
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
  [ dir "api"    $ apiRoute
  , dir "static" $ serveDirectory DisableBrowsing ["index.html"] "static"
  , pageRoute
  , notFoundResponse
  ]

--
-- Html pages
--

pageRoute :: ServerT Response
pageRoute = msum
  [ do nullDir
       uid <- getUserID
       ok $ toResponse $ indexPage (recentPosts uid)
  ]

--
-- JS api
--

apiRoute :: ServerT Response
apiRoute = mzero

--
-- Error responses
--

internalServerErrorResponse :: IOException -> ServerT Response
internalServerErrorResponse =
  internalServerError . toResponse . page500InternalError

notFoundResponse :: ServerT Response
notFoundResponse = notFound $ toResponse $ page404NotFound
