module Main where

import Control.Monad

-- happstack package
import Happstack.Server

-- html pages
import Html.Error
import Html.Index

main :: IO ()
main = do
  simpleHTTP conf mainRoute
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

notFoundResponse :: ServerPart Response
notFoundResponse = notFound $ toResponse $ page404NotFound
