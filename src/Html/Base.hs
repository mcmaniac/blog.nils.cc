{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Html.Base where

import Data.Foldable
import Data.Monoid

import qualified Data.Map as Map

-- text package
import Data.Text as Text

-- lens package
import Control.Lens
import Data.Text.Lens

-- blaze-html package
import Text.Blaze.Html (Html, (!), toValue)
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

-- happstack packages
import Happstack.Server.Response

-- i18n
import Text.I18n
import Text.Blaze.I18n

-- our modules
import Html.Helper

--
-- HtmlPage definition
--

type Link = (Text, Text, Text)

data HtmlPage = HtmlPage
  { _pageTitle    :: Text
  , _pageName     :: Maybe Text
  , _pageScripts  :: [FilePath]
  , _pageStyles   :: [FilePath]
  , _pageLinks    :: [Link]
  , _pageLocale   :: Maybe Locale
  , _pageL10n     :: L10n
  , _pageHeader   :: Maybe Html
  , _pageBody     :: Html
  , _pageFooter   :: Maybe Html
  }

makeLenses ''HtmlPage

emptyPage :: HtmlPage
emptyPage = HtmlPage
  { _pageTitle = ""
  , _pageName = Nothing
  , _pageScripts = []
  , _pageStyles = []
  , _pageLinks = []
  , _pageLocale = Nothing
  , _pageL10n = Map.empty
  , _pageHeader = Nothing
  , _pageBody = mempty
  , _pageFooter = Nothing
  }

setLocalization :: HtmlPage -> L10n -> Locale -> HtmlPage
setLocalization page l10n loc = page &~ do
  pageLocale .= Just loc
  pageL10n   .= l10n

basePage :: HtmlPage
basePage = emptyPage &~ do
  pageTitle  .= "blog.nils.cc"
  pageStyles .= [ "base.css" ]
  pageLinks  .= [ ubuntuFont  ]
 where
  ubuntuFont = ("text/css", "http://fonts.googleapis.com/css?family=Ubuntu", "stylesheet")

-- | Render a HtmlPage as Html
renderPage :: HtmlPage -> Html
renderPage page = localizeMarkup l10 loc $ H.docTypeHtml $ do

  H.head $ i18nContext "html-head" $ do

    H.title $ do
      i18n $ page ^. pageTitle . _Text
      maybe (return ()) `flip` (page ^? pageName . _Just . _Text) $ \name -> do
        " - "
        i18n name

    -- load javascript
    forM_ (page ^. pageScripts) $ \s ->
      H.script ! A.type_ "text/javascript" !
        A.src (toValue $ "/static/js/" ++ s) $ mempty

    -- load css
    forM_ (page ^. pageStyles) $ \c ->
      H.link ! A.type_ "text/css" !
        A.href (toValue $ "/static/css/" ++ c) ! A.rel "stylesheet"

    -- external links
    forM_ (page ^. pageLinks) $ \(ty, href, rel) ->
      H.link ! A.type_ (ty   ^. value)
             ! A.href  (href ^. value)
             ! A.rel   (rel  ^. value)

  H.body $ do

    forM_ (page ^. pageHeader) $ \hdr -> do
      H.div ! A.id "main-header" $ hdr

    H.div ! A.id "main-body" $
      page ^. pageBody

    forM_ (page ^. pageFooter) $ \ftr -> do
      H.div ! A.id "main-footer" $ ftr
 where
  loc = page ^. pageLocale . non (Locale "en")
  l10 = page ^. pageL10n

instance ToMessage HtmlPage where
  toContentType _ = toContentType (mempty :: Html)
  toMessage     p = toMessage $ renderPage p

--
-- HTML helper
--

input :: String -> String -> Html -> Html
input ty name lbl = do
  H.label $ lbl
  H.input ! A.type_ (toValue ty) ! A.name (toValue name)

textInput, passInput :: String -> Html -> Html
textInput = input "text"
passInput = input "password"
