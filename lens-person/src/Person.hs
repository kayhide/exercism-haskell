{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Optics

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }

makeFieldLabelsWith classUnderscoreNoPrefixFields ''Person
makeFieldLabelsWith classUnderscoreNoPrefixFields ''Name
makeFieldLabelsWith classUnderscoreNoPrefixFields ''Born
makeFieldLabelsWith classUnderscoreNoPrefixFields ''Address

bornStreet :: Born -> String
bornStreet = view $ #bornAt % #street

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (#address % #street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth = set (#born % #bornOn % _month)

_month :: Lens' Day Int
_month =
  lens
  (view _2 . toGregorian)
  (\day m -> case toGregorian day of (y, _, d) -> fromGregorian y m d)


renameStreets :: (String -> String) -> Person -> Person
renameStreets f = over (#address % #street) f . over (#born % #bornAt % #street) f
