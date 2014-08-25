{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Html.Base where

import Data.Monoid
import Control.Monad

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

-- our modules
import Html.Helper

--
-- HtmlPage definition
--

type Link = (Text, Text, Text)

data HtmlPage = HtmlPage
  { _pageTitle   :: Text
  , _pageName    :: Maybe Text
  , _pageScripts :: [FilePath]
  , _pageStyles  :: [FilePath]
  , _pageLinks   :: [Link]
  , _pageBody    :: Html
  }

makeLenses ''HtmlPage

emptyPage :: HtmlPage
emptyPage = HtmlPage "" Nothing [] [] [] mempty

basePage :: HtmlPage
basePage = emptyPage
  & pageTitle   .~ "blog.nils.cc"
  & pageStyles  .~ [ "base.css" ]
  & pageLinks   .~ [ ubuntuFont  ]
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

    page ^. pageBody

instance ToMessage HtmlPage where
  toContentType _ = toContentType (mempty :: Html)
  toMessage     p = toMessage $ renderPage p
