module Main where

import Control.Monad
import Happstack.Server

-- html pages
import Html.Error

happstackConfig :: Conf
happstackConfig = nullConf { port = 8686 }

main :: IO ()
main = simpleHTTP happstackConfig mainRoute

mainRoute :: ServerPart Response
mainRoute = msum
  [ dir "api"    $ apiRoute
  , dir "static" $ serveDirectory DisableBrowsing ["index.html"] "static"
  , notFoundResponse
  ]

apiRoute :: ServerPart Response
apiRoute = mzero

notFoundResponse :: ServerPart Response
notFoundResponse = notFound $ toResponse $ page404NotFound
