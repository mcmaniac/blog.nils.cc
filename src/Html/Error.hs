{-# Language OverloadedStrings #-}

module Html.Error where

-- lens package
import Control.Lens

-- blaze-html package
import Text.Blaze.Html (Html, (!), toValue)
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

-- our modules
import Html.Helper
import Html.Base

page404NotFound :: HtmlPage
page404NotFound = basePage & pageBody .~ do
  H.p "Page not found."
