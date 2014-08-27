module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Error

-- happstack package
import Data.Acid
import Happstack.Server
import Happstack.Server.ClientSession

import Text.I18n.Po (L10n, getL10n)

-- html pages
import Html.Error

import Session
import State
import Routes

main :: IO ()
main = do

  key  <- getDefaultKey
  acid <- openLocalState emptyBlogState
  (l10n,err) <- getL10n "lang"

  when (not $ null err) $ do
    putStrLn "Warning: Error parsing language files:"
    mapM_ print err

  putStrLn $ "Starting server on " ++ (show $ port hsconf)

  simpleHTTP hsconf $
    runServerT acid (sessionconf key) $
      mainRoute l10n `catchError` internalServerErrorResponse

 where

  -- happstack config
  hsconf = nullConf { port = 8686 }

  -- session config
  sessionconf key = (mkSessionConf key)
    { sessionCookieLife = oneMonth
    , sessionCookieName = "blog.user.session"
    }

  oneMonth        = MaxAge $ 60 * 60 * 24 * 7 * 4

--
-- Start routing
--

mainRoute :: L10n -> ServerT Response
mainRoute l10n = msum
  [ dir "static" $ serveDirectory DisableBrowsing ["index.html"] "static"
  , dir "api"    $ apiRoute
  , pageRoute l10n
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
