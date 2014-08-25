module Routes where

import Control.Monad

-- lens
import Control.Lens

-- happstack
import Happstack.Server

-- local
import Session

import Html.Base
import Html.Index

import Routes.Header
import Routes.Footer

--
-- Html pages
--

pageRoute :: ServerT Response
pageRoute = do

  hdr_html <- buildHeaderHtml

  page <- msum

    [ do nullDir
         _mu <- getSessionUser
         return $ indexPage
    ]

  ftr_html <- buildFooterHtml

  ok $ toResponse $ page & pageBody .~ do

    hdr_html

    page ^. pageBody

    ftr_html

--
-- Ajax api
--

apiRoute :: ServerT Response
apiRoute = mzero
