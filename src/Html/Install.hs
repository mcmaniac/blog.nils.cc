{-# LANGUAGE OverloadedStrings #-}

module Html.Install where

-- lens package
import Control.Lens
import Data.Text.Lens

-- blaze-html package
import Text.Blaze.Html (Html, (!), toValue)
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

-- local modules
import Html.Base

installPage :: HtmlPage
installPage = basePage
  & pageName  .~ Just "Install"
  & pageBody  .~ do

    installHtml

installHtml :: Html
installHtml = do

  H.h1 "Welcome!"
