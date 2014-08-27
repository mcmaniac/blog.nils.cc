module Routes where

import Control.Monad

-- lens
import Control.Lens

-- happstack
import Happstack.Server

import Text.I18n

-- local
import Session
import State

import Html.Base

import Routes.Header
import Routes.Footer

import Routes.Install
import Routes.Index
import Routes.Config
import Routes.Post

--
-- Html pages
--

pageRoute :: L10n -> ServerT Response
pageRoute l10n = do

  page <- msum [ buildInstallPage
               , buildIndexPage
               , buildConfigPage
               , buildPostPage
               ]

  loc <- getLocale

  ok . toResponse =<< buildMainPage (setLocalization page l10n loc)

 where
  getLocale = msum
    [ getSessionLocale          >>= maybe mzero return
    , runQuery GetDefaultLocale >>= maybe mzero (return . Locale)
    , return $ Locale "en"
    ]

buildMainPage :: HtmlPage -> ServerT HtmlPage
buildMainPage page = do

  hdr <- maybe buildHeaderHtml return $ page ^. pageHeader
  ftr <- maybe buildFooterHtml return $ page ^. pageFooter

  return $ page &~ do
    pageHeader .= Just hdr
    pageFooter .= Just ftr


--
-- Ajax api
--

apiRoute :: ServerT Response
apiRoute = mzero
