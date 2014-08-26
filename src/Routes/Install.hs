module Routes.Install ( buildInstallPage ) where

import Control.Monad

import Text.Blaze.Html

-- local
import Session
import State

import Html.Base
import Html.Install

buildInstallPage :: ServerT HtmlPage
buildInstallPage = do

  -- see if configuration is available
  mconf <- runQuery GetConfig
  case mconf of
    Just _  -> mzero
    Nothing -> build'

build' :: ServerT HtmlPage
build' = do
  mu <- getSessionUser
  return $ installPage mu
