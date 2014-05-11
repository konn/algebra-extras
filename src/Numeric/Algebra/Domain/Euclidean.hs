module Numeric.Algebra.Domain.Euclidean where
import           Control.Lens
import           Numeric.Algebra
import           Numeric.Algebra.Domain
import           Numeric.Decidable.Zero
import           Prelude                (Eq (..), Maybe (..), abs, fst, head,
                                         otherwise, snd, (.))
import           Prelude                (Integer, fromIntegral, ($))
import qualified Prelude                as P

infixl 7 `quot`, `rem`
infix  7 `divide`
class (DecidableZero r, Domain r) => Euclidean r where
  -- | Euclidean (degree) function on @r@.
  degree :: r -> Maybe Natural
  -- | Division algorithm. @a `divide` b@ calculates
  --   quotient and reminder of @a@ divided by @b@.
  --
  -- prop> divide a p == (q, r) ==> p*q + r == a && degree r < degree q
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
  euclid f g = step [(g, zero, one), (f, one, zero)]
    where
      step acc @ ((r',s',t'):(r,s,t):_)
        | isZero r' = P.tail acc
        | otherwise =
          let q   = r `quot` r'
              r'' = r - q * r'
              s'' = s - q * s'
              t'' = t - q * t'
          in step ((r'', s'', t'') : acc)
  {-# MINIMAL degree, divide #-}

instance Euclidean Integer where
  degree = Just . fromIntegral . abs
  divide = P.divMod
