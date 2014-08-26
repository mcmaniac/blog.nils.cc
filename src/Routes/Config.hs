module Routes.Config where

import Text.Blaze.Html

-- local
import Session

import Html.Base
import Html.Config

buildConfigPage :: ServerT HtmlPage
buildConfigPage = do

  return basePage

