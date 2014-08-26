module Routes.Install where

import Text.Blaze.Html

-- local
import Session

import Html.Base
import Html.Install

buildInstallPage :: ServerT HtmlPage
buildInstallPage = do

  return basePage

