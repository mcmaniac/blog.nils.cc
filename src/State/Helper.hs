{-# LANGUAGE RankNTypes #-}

module State.Helper where

import Control.Lens

import Data.Typeable
import Data.IxSet as IxS

import Text.I18n

--
-- Some prism/lenses
--

locale :: Lens' String Locale
locale = lens Locale (\_ (Locale s) -> s)

--
-- IxSet helper
--

ixSetAt
  :: (Typeable k, Typeable a, IxS.Indexable a, Ord a)
  => k
  -> Lens' (IxSet a) (Maybe a)
ixSetAt key = lens
  (getOne . (@= key))
  (\db val -> case val of
    Just a  -> updateIx key a db
    Nothing -> deleteIx key   db
  )

list :: (Ord a, Typeable a, IxS.Indexable a) => Iso' (IxSet a) [a]
list = iso toList fromList
