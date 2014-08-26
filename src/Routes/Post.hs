module Routes.Post where

import Control.Monad

import Text.Blaze.Html

-- local
import Session

import Html.Base
import Html.Post

buildPostPage :: ServerT HtmlPage
buildPostPage = mzero
