module Main where

import Control.Monad

-- happstack package
import Happstack.Server

-- html pages
import Html.Error

main :: IO ()
main = simpleHTTP conf mainRoute
 where
  conf = nullConf { port = 8686 }

mainRoute :: ServerPart Response
mainRoute = msum
  [ dir "api"    $ apiRoute
  , dir "static" $ serveDirectory DisableBrowsing ["index.html"] "static"
  , notFoundResponse
  ]

--
-- JS api
--

apiRoute :: ServerPart Response
apiRoute = mzero

--
-- Error responses
--

notFoundResponse :: ServerPart Response
notFoundResponse = notFound $ toResponse $ page404NotFound
