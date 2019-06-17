module SpaceAge (Planet(..), ageOn) where

data Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune

ageOnEarth :: Float -> Float
ageOnEarth = (/ 31557600)

ageOn :: Planet -> Float -> Float
ageOn Earth   = ageOnEarth
ageOn Mercury = ageOnEarth . (/ 0.2408467)
ageOn Venus   = ageOnEarth . (/ 0.61519726)
ageOn Mars    = ageOnEarth . (/ 1.8808158)
ageOn Jupiter = ageOnEarth . (/ 11.862615)
ageOn Saturn  = ageOnEarth . (/ 29.447498)
ageOn Uranus  = ageOnEarth . (/ 84.016846)
ageOn Neptune = ageOnEarth . (/ 164.79132)
