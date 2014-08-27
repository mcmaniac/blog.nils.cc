{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Html.Base where

import Data.Foldable
import Data.Monoid

-- text package
import Data.Text as Text

-- lens package
import Control.Lens

-- blaze-html package
import Text.Blaze.Html (Html, (!), toMarkup, toValue)
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
  , _pageHeader   :: Maybe Html
  , _pageBody     :: Html
  , _pageFooter   :: Maybe Html
  }

makeLenses ''HtmlPage

emptyPage :: HtmlPage
emptyPage = HtmlPage "" Nothing [] [] [] Nothing mempty Nothing

localizePage :: L10n -> Locale -> HtmlPage -> HtmlPage
localizePage l10n loc page = page &~ do
  pageHeader %= fmap (localizeMarkup l10n loc)
  pageBody   %=       localizeMarkup l10n loc
  pageFooter %= fmap (localizeMarkup l10n loc)

basePage :: HtmlPage
basePage = emptyPage &~ do
  pageTitle  .= "blog.nils.cc"
  pageStyles .= [ "base.css" ]
  pageLinks  .= [ ubuntuFont  ]
 where
  ubuntuFont = ("text/css", "http://fonts.googleapis.com/css?family=Ubuntu", "stylesheet")

-- | Render a HtmlPage as Html
renderPage :: HtmlPage -> Html
renderPage page = H.docTypeHtml $ do

  H.head $ do

    H.title $ do
      toMarkup $ page ^. pageTitle
      toMarkup $ maybe "" (" - " `Text.append`) (page ^. pageName)

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

instance ToMessage HtmlPage where
  toContentType _ = toContentType (mempty :: Html)
  toMessage     p = toMessage $ renderPage p

--
-- HTML helper
--

input :: String -> String -> Text -> Html
input name ty labl = do
  H.label $ do
    toMarkup $ labl
    " "
    H.input ! A.type_ (toValue ty) ! A.name (toValue name)

