module Routes.Header where

import Text.Blaze.Html

-- local
import Session
import Html.Header

buildHeaderHtml :: ServerT Html
buildHeaderHtml = do

  return $ headerHtml

