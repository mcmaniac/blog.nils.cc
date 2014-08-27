{-# LANGUAGE OverloadedStrings #-}

module Html.Install
  ( installPage
  , installHtml
  ) where

-- lens package
import Control.Lens
import Data.Text.Lens

-- blaze-html package
import Text.Blaze.Html (Html, (!), toValue)
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

import Text.Blaze.I18n

-- local modules
import Html.Base

import State.Users

installPage :: Maybe User -> HtmlPage
installPage muser = basePage
  & pageName  .~ Just "Install"
  & pageBody  .~ do

    installHtml muser

installHtml :: Maybe User -> Html
installHtml mu = H.div ! A.id "install" $ do

  H.h1 $ i18n "Welcome!"

  H.div ! A.class_ "new-user" $ do
    case mu of
      Nothing -> newUserForm
      Just _  -> return ()

newUserForm :: Html
newUserForm = do
  H.form ! A.action "/" ! A.method "post" $ do
    H.p $ i18n "Please create a new user:"
    textInput "username" $ i18n "Username:"
    passInput "password" $ i18n "Password:"
