module Numeric.Algebra.Domain.Euclidean where
import           Control.Lens
import           Numeric.Algebra
import           Numeric.Algebra.Domain
import           Numeric.Decidable.Zero
import           Prelude                (Eq (..), Maybe (..), abs, fst, head,
                                         otherwise, snd, (.))
import           Prelude                (Integer, fromIntegral, ($))
import qualified Prelude                as P

class (DecidableZero r, Domain r) => Euclidean r where
  -- | Euclidean (degree) function on @r@.
  degree :: r -> Maybe Natural
  -- | Division algorithm.

  divide :: r                   -- ^ elements divided by
         -> r                   -- ^ divisor
         -> (r                  -- ^ quotient
            ,r                  -- ^ reminder
            )
  div :: r -> r -> r
  div a b = fst $ a `divide` b
  mod :: r -> r -> r
  mod a b = snd $ a `divide` b
  gcd :: r -> r -> r
  gcd a b = head (euclid a b)^._1
  -- | Extended euclidean algorithm.
  euclid :: r -> r -> [(r,r,r)]
  euclid f g = step [(g, zero, one), (f, one, zero)]
    where
      step acc @ ((r',s',t'):(r,s,t):_)
        | isZero r' = P.tail acc
        | otherwise =
          let q   = r `div` r'
              r'' = r - q * r'
              s'' = s - q * s'
              t'' = t - q * t'
          in step ((r'', s'', t'') : acc)

instance Euclidean Integer where
  degree = Just . fromIntegral . abs
  divide = P.divMod
