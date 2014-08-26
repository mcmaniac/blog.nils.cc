module Routes.Index where

import Text.Blaze.Html

-- local
import Session

import Html.Base
import Html.Index

buildIndexPage :: ServerT HtmlPage
buildIndexPage = do

  return basePage
