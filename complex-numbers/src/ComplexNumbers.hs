module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (abs, div, exp)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a
  deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex = uncurry Complex

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex r i) = complex (r, negate i)

abs :: Floating a => Complex a -> a
abs (Complex r i) = sqrt $ r * r + i * i

real :: Num a => Complex a -> a
real (Complex r _) = r

imaginary :: Num a => Complex a -> a
imaginary (Complex _ i) = i

exp :: Floating a => Complex a -> Complex a
exp (Complex r i) = complex (P.exp r * cos i, P.exp r * sin i)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex xr xi) (Complex yr yi) = complex (xr * yr - xi * yi, xr * yi + xi * yr)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex xr xi) (Complex yr yi) = complex (xr + yr, xi + yi)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex xr xi) (Complex yr yi) = complex (xr - yr, xi - yi)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex xr xi) (Complex yr yi) = complex (r', i')
  where
    r' = (xr * yr + xi * yi) / d'
    i' = (xi * yr - xr * yi) / d'
    d' = yr * yr + yi * yi
