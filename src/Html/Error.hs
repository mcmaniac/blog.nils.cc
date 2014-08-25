{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Html.Error where

import Control.Exception

-- lens package
import Control.Lens

-- blaze-html package
import Text.Blaze.Html (Html, (!))
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

-- our modules
import Html.Helper
import Html.Base

page404NotFound :: HtmlPage
page404NotFound = basePage & pageBody .~ do

  H.h1 "Page not found."
  H.p "The page you requested could not be found."
  H.p $ do
    "Go back to "
    H.a ! A.href "/" $ "home"
    "."

page500InternalError :: IOException -> HtmlPage
page500InternalError exception = basePage & pageBody .~ do

  H.h1 "Internal server error."
  H.p "The error message was:"
  H.pre $ show exception ^. markup
