module Routes.Post where

import Text.Blaze.Html

-- local
import Session

import Html.Base
import Html.Post

buildPostPage :: ServerT HtmlPage
buildPostPage = do

  return basePage

