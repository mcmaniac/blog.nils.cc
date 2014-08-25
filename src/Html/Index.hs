{-# LANGUAGE OverloadedStrings #-}

module Html.Index where

-- lens package
import Control.Lens

-- blaze-html package
import Text.Blaze.Html (Html, (!), toValue)
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

-- our modules
import Html.Helper
import Html.Base

indexPage :: Html -> HtmlPage
indexPage html = basePage
  & pageName  .~ Just "Home"
  & pageBody  .~ do

    H.p "ok"

    html
