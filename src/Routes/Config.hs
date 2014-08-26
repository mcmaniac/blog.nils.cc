module Routes.Config where

import Control.Monad

import Text.Blaze.Html

-- local
import Session

import Html.Base
import Html.Config

buildConfigPage :: ServerT HtmlPage
buildConfigPage = mzero
