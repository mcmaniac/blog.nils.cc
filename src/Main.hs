module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Error

-- happstack package
import Happstack.Server

-- html pages
import Html.Error
import Html.Index

main :: IO ()
main = do
  simpleHTTP conf $ mainRoute `catchError` internalServerErrorResponse
 where
  conf = nullConf { port = 8686 }

mainRoute :: ServerPart Response
mainRoute = msum
  [ dir "api"    $ apiRoute
  , dir "static" $ serveDirectory DisableBrowsing ["index.html"] "static"
  , pageRoute
  , notFoundResponse
  ]

--
-- Html pages
--

pageRoute :: ServerPart Response
pageRoute = msum
  [ do nullDir
       ok $ toResponse $ indexPage (return ())
  ]

--
-- JS api
--

apiRoute :: ServerPart Response
apiRoute = mzero

--
-- Error responses
--

internalServerErrorResponse :: IOException -> ServerPart Response
internalServerErrorResponse =
  internalServerError . toResponse . page500InternalError

notFoundResponse :: ServerPart Response
notFoundResponse = notFound $ toResponse $ page404NotFound
