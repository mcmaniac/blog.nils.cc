module Routes.Footer where

import Text.Blaze.Html

-- local
import Session
import Html.Footer

buildFooterHtml :: ServerT Html
buildFooterHtml = do

  return $ footerHtml
