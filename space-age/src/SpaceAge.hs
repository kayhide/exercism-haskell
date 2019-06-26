module SpaceAge (Planet(..), ageOn) where

data Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune

newtype EarthYear = EarthYear Float

type Seconds = Float
type PlanetYear = Float

getSeconds :: EarthYear -> Seconds
getSeconds (EarthYear x) = x * 60 * 60 * 24 * 365.25

orbitalPeriodOf :: Planet -> EarthYear
orbitalPeriodOf Earth   = EarthYear 1.0
orbitalPeriodOf Mercury = EarthYear 0.2408467
orbitalPeriodOf Venus   = EarthYear 0.61519726
orbitalPeriodOf Mars    = EarthYear 1.8808158
orbitalPeriodOf Jupiter = EarthYear 11.862615
orbitalPeriodOf Saturn  = EarthYear 29.447498
orbitalPeriodOf Uranus  = EarthYear 84.016846
orbitalPeriodOf Neptune = EarthYear 164.79132

ageOn :: Planet -> Seconds -> PlanetYear
ageOn planet = (/ getSeconds (orbitalPeriodOf planet))
