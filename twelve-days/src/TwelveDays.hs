{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module TwelveDays (recite) where

import Data.Proxy
import GHC.TypeLits

recite :: Int -> Int -> [String]
recite start stop = verse <$> [start .. stop]

verse :: Int -> String
verse = \case
  1  -> symbolVal (Proxy @(Verse 1))
  2  -> symbolVal (Proxy @(Verse 2))
  3  -> symbolVal (Proxy @(Verse 3))
  4  -> symbolVal (Proxy @(Verse 4))
  5  -> symbolVal (Proxy @(Verse 5))
  6  -> symbolVal (Proxy @(Verse 6))
  7  -> symbolVal (Proxy @(Verse 7))
  8  -> symbolVal (Proxy @(Verse 8))
  9  -> symbolVal (Proxy @(Verse 9))
  10 -> symbolVal (Proxy @(Verse 10))
  11 -> symbolVal (Proxy @(Verse 11))
  12 -> symbolVal (Proxy @(Verse 12))
  _  -> error "Bad verse"

type family Verse (a :: Nat) where
  Verse n =
    ConcatSymbols
    [ "On the "
    , Ordinal n
    , " day of Christmas my true love gave to me: "
    , Items n
    , " in a Pear Tree."
    ]

type family Items (a :: Nat) where
  Items 1 = Item 1
  Items 2 = ConcatSymbols [Item 2, ", and ", Items 1]
  Items n = ConcatSymbols [Item n, ", ", Items (n - 1)]

type family ConcatSymbols (as :: [Symbol]) where
  ConcatSymbols '[] = ""
  ConcatSymbols (x : xs) = AppendSymbol x (ConcatSymbols xs)

type family Item (a :: Nat) where
  Item 1 = "a Partridge"
  Item 2 = "two Turtle Doves"
  Item 3 = "three French Hens"
  Item 4 = "four Calling Birds"
  Item 5 = "five Gold Rings"
  Item 6 = "six Geese-a-Laying"
  Item 7 = "seven Swans-a-Swimming"
  Item 8 = "eight Maids-a-Milking"
  Item 9 = "nine Ladies Dancing"
  Item 10 = "ten Lords-a-Leaping"
  Item 11 = "eleven Pipers Piping"
  Item 12 = "twelve Drummers Drumming"

type family Ordinal (a :: Nat) where
  Ordinal 1 = "first"
  Ordinal 2 = "second"
  Ordinal 3 = "third"
  Ordinal 4 = "fourth"
  Ordinal 5 = "fifth"
  Ordinal 6 = "sixth"
  Ordinal 7 = "seventh"
  Ordinal 8 = "eighth"
  Ordinal 9 = "ninth"
  Ordinal 10 = "tenth"
  Ordinal 11 = "eleventh"
  Ordinal 12 = "twelfth"
