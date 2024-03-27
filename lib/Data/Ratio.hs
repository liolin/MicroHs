module Data.Ratio(
  Ratio, Rational,
  (%),
  numerator, denominator,
  rationalInfinity,
  rationalNaN,
  rationalMinusZero,
  ) where
import Prelude()              -- do not import Prelude
import Data.Bool
import Data.Eq
import Data.Fractional
import Data.Function
import Data.Integer
import Data.Integral
import Data.Num
import Data.Ord
import Data.Ratio_Type
import Text.Show

{- in Data.Ratio_Type
data Ratio a = !a :% !a
-}
-- The value x :% y represents the rational number x/y.
-- The y is always >= 0, and the number is in reduced for,
-- i.e., there are no common factor > 1 for x and y.
-- In addition to the ordinary rationals, we use the "wheel"
-- extension of the rationals.
-- This means that the / operation is total.  In particular
--  x/0 == 1/0 == rationalInfinity, when x/=0
--  0/0 == rationalNaN (often called perp for wheels).
-- Wheels obey most of the usual algebraic laws for rationals,
-- but the distributive law has to be augmented to work for all
-- numbers.
--  (x + y) * z + 0*z == x*z + y*z
-- When z is a normal rational number the is clearly the same
-- as the usual distributive law.
-- 
-- NOTE. Experimentally, we extend this with:
--  x/0 ==  1/0 when x > 0
--  x/0 == -1/0 when x < 0

instance forall a . Eq a => Eq (Ratio a) where
  (x :% y) == (x' :% y')  =  x == x' && y == y'

instance forall a . (Integral a, Ord a) => Ord (Ratio a) where
  (x :% y) <= (x' :% y')  =  x * y' <= x' * y
  (x :% y) <  (x' :% y')  =  x * y' <  x' * y
  (x :% y) >= (x' :% y')  =  x * y' >= x' * y
  (x :% y) >  (x' :% y')  =  x * y' >  x' * y

instance forall a . (Integral a, Ord a) => Num (Ratio a) where
  (x:%y) + (x':%y')   =  reduce (x*y' + x'*y) (y*y')
  (x:%y) - (x':%y')   =  reduce (x*y' - x'*y) (y*y')
  (x:%y) * (x':%y')   =  reduce (x * x') (y * y')
  negate (x:%y)       =  negate x :% y
  abs (x:%y)          =  abs x :% y
  signum (x:%_)       =  signum x :% 1
  fromInteger x       =  fromInteger x :% 1

instance forall a . (Integral a, Ord a) => Fractional (Ratio a) where
  (x:%y) / (x':%y')   = reduce (x*y') (y*x')
  recip (x:%y)
    | x < 0           = (-y) :% (-x)
    | otherwise       =  y :%  x
  fromRational (x:%y) =  fromInteger x % fromInteger y

instance forall a . (Show a) => Show (Ratio a)  where
  showsPrec p (x:%y) = showParen (p > 7) $
                       showsPrec 8 x .
                       showString " % " .
                       showsPrec 8 y

rationalInfinity :: Rational
rationalInfinity = 1 :% 0

rationalNaN :: Rational
rationalNaN = 0 :% 0

rationalMinusZero :: Rational
rationalMinusZero = 0 :% (-1)

infixl 7 %
(%) :: forall a . (Integral a, Ord a) => a -> a -> Ratio a
x % y = reduce x y

reduce :: forall a . (Ord a, Integral a) => a -> a -> Ratio a
reduce x y | y > 0 = let d = gcd x y in (x `quot` d) :% (y `quot` d)
           | y < 0 = reduce (- x) (- y)
           | otherwise = signum x :% 0

numerator :: forall a . Ratio a -> a
numerator (x :% _) = x

denominator :: forall a . Ratio a -> a
denominator (_ :% y) = y
