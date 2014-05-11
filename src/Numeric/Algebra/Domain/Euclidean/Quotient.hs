{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, UndecidableInstances         #-}
module Numeric.Algebra.Domain.Euclidean.Quotient (Quotient(), getQuotient,
                                                  reifyQuotient, withModulus) where
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

import Numeric.Algebra.Domain.Euclidean

newtype Quotient a m = Quotient { getMod_ :: a
                                } deriving (Commutative, Monoidal, Rig, Unital,
                                            Rng, Semiring, Abelian, Group, P.Num)

getQuotient :: Quotient a m -> a
getQuotient = getMod_

instance (Euclidean r, Reifies m r) => Additive (Quotient r m) where
  m@(Quotient a) + Quotient b = Quotient $ (a + b) `rem` reflect m
  {-# INLINE (+) #-}

instance (Euclidean r, Reifies m r) => Multiplicative (Quotient r m) where
  m@(Quotient a) * Quotient b = Quotient $ (a * b) `rem` reflect m
  {-# INLINE (*) #-}

instance (Euclidean r, Reifies m r) => LeftModule Natural (Quotient r m) where
  n .* m@(Quotient a) = Quotient $ (n .* a) `rem` reflect m
  {-# INLINE (.*) #-}

instance (Euclidean r, Reifies m r) => RightModule Natural (Quotient r m) where
  m@(Quotient a) *. n = Quotient $ (a *. n) `rem` reflect m
  {-# INLINE (*.) #-}

instance (Euclidean r, Reifies m r) => LeftModule Integer (Quotient r m) where
  n .* m@(Quotient a) = Quotient $ (n .* a) `rem` reflect m
  {-# INLINE (.*) #-}

instance (Euclidean r, Reifies m r) => RightModule Integer (Quotient r m) where
  m@(Quotient a) *. n = Quotient $ (a *. n) `rem` reflect m
  {-# INLINE (*.) #-}

instance (Euclidean r, Reifies m r) => DecidableUnits (Quotient r m)  where
  recipUnit m@(Quotient a) =
    let (g, i, _) : _ = euclid a (reflect m)
    in if isZero (g - one) then Just (Quotient $ i `rem` reflect m) else Nothing
  {-# INLINE recipUnit #-}

reifyQuotient :: r -> (forall m. Reifies m r => Quotient r m) -> r
reifyQuotient r f = reify r $ getMod_ . withModulus f

withModulus :: Quotient r m -> p m -> Quotient r m
withModulus = P.const

-- | WARNING: This instance should be used only when modulo @m@ is prime number.
instance Reifies m Integer => Division (Quotient Integer m) where
  recip = fromJust . recipUnit
