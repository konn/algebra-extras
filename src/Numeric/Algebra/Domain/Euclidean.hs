{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, UndecidableInstances         #-}
module Numeric.Algebra.Domain.Euclidean (Euclidean(..), gcd', leadingUnit, normalize) where
import           Control.Arrow
import           Control.Lens
import           Data.Maybe              (fromJust)
import           Data.Reflection
import           Numeric.Algebra
import           Numeric.Algebra.Domain
import           Numeric.Decidable.Units
import           Numeric.Decidable.Zero
import           Prelude                 (Eq (..), Integer, Maybe (..), abs)
import           Prelude                 (fromIntegral, fst, head, otherwise)
import           Prelude                 (signum, snd, ($), (.))
import qualified Prelude                 as P

infixl 7 `quot`, `rem`
infix  7 `divide`
class (DecidableZero r, DecidableUnits r, Domain r) => Euclidean r where
  -- | @splitUnit r@ calculates its leading unit and normal form.
  --
  -- prop> splitUnit r == (u, n) ==> r == u * n && fst (splitUnit n) == one && isUnit u
  splitUnit :: r -> (r, r)
  -- | Euclidean (degree) function on @r@.
  degree :: r -> Maybe Natural
  -- | Division algorithm. @a `divide` b@ calculates
  --   quotient and reminder of @a@ divided by @b@.
  --
  -- prop> let (q, r) = divide a p in p*q + r == a && degree r < degree q
  divide :: r                   -- ^ elements divided by
         -> r                   -- ^ divisor
         -> (r,r)               -- ^ quotient and remin
  quot :: r -> r -> r
  quot a b = fst $ a `divide` b
  rem :: r -> r -> r
  rem a b = snd $ a `divide` b
  -- | @'gcd' a b@ calculates greatest common divisor of @a@ and @b@.
  gcd :: r -> r -> r
  gcd a b = head (euclid a b)^._1
  -- | Extended euclidean algorithm.
  --
  -- prop> euclid f g == xs ==> all (\(r, s, t) -> r == f * s + g * t) xs
  euclid :: r -> r -> [(r,r,r)]
  euclid f g =
    let (ug, g') = splitUnit g
        Just t'  = recipUnit ug
        (uf, f') = splitUnit f
        Just s   = recipUnit uf
    in step [(g', zero, t'), (f', s, zero)]
    where
      step acc @ ((r',s',t'):(r,s,t):_)
        | isZero r' = P.tail acc
        | otherwise =
          let q         = r `quot` r'
              (ur, r'') = splitUnit $ r - q * r'
              Just u    = recipUnit ur
              s''       = (s - q * s') * u
              t''       = (t - q * t') * u
          in step ((r'', s'', t'') : acc)
  {-# MINIMAL splitUnit, degree, divide #-}

gcd' :: Euclidean r => [r] -> r
gcd' []     = one
gcd' [x]    = leadingUnit x
gcd' [x,y]  = gcd x y
gcd' (x:xs) = gcd x (gcd' xs)

normalize :: Euclidean r => r -> r
normalize = snd . splitUnit

leadingUnit :: Euclidean r => r -> r
leadingUnit = fst . splitUnit

instance Euclidean Integer where
  splitUnit 0 = (1, 0)
  splitUnit n = (signum n, abs n)
  degree = Just . fromIntegral . abs
  divide = P.divMod
