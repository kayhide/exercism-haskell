{-# LANGUAGE LambdaCase #-}
module SpaceAge (Planet(..), ageOn) where

data Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune
  deriving (Eq, Show)

type Seconds = Float
type PlanetYear = Float

newtype EarthYear = EarthYear Float
  deriving (Eq, Show)

data SecondsOn = SecondsOn Planet Seconds
  deriving (Eq, Show)


orbitalPeriodOf :: Planet -> EarthYear
orbitalPeriodOf = EarthYear . \case
  Earth   -> 1.0
  Mercury -> 0.2408467
  Venus   -> 0.61519726
  Mars    -> 1.8808158
  Jupiter -> 11.862615
  Saturn  -> 29.447498
  Uranus  -> 84.016846
  Neptune -> 164.79132

ageOn :: Planet -> Seconds -> PlanetYear
ageOn planet = getPlanetYear . SecondsOn planet

getPlanetYear :: SecondsOn -> PlanetYear
getPlanetYear (SecondsOn planet seconds) = seconds / getSeconds (orbitalPeriodOf planet)

getSeconds :: EarthYear -> Seconds
getSeconds (EarthYear x) = x * 60 * 60 * 24 * 365.25
