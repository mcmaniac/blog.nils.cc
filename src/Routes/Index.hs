module Routes.Index where

import Control.Monad

import Text.Blaze.Html

-- local
import Session

import Html.Base
import Html.Index

buildIndexPage :: ServerT HtmlPage
buildIndexPage = mzero
