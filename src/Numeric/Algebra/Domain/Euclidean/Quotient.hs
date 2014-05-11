{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, UndecidableInstances         #-}
module Numeric.Algebra.Domain.Euclidean.Quotient (Quotient(), mod, getQuotient,
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
import           Prelude                 (Read (..), Show (..), signum, snd)
import           Prelude                 (showChar, showParen, showString, ($),
                                          (.))
import qualified Prelude                 as P

import Numeric.Algebra.Domain.Euclidean

newtype Quotient a m = Quotient { getMod_ :: a
                                } deriving (Commutative, Monoidal, Rig, Unital, Eq,
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

infix 7 `mod`
mod :: (Euclidean r, Reifies m r) => r -> p m -> Quotient r m
mod x p = Quotient $ x `rem` reflect p

modulus :: Reifies m r => Quotient r m -> r
modulus = reflect

data Proxy a = Proxy

instance (Euclidean r, Read r, Reifies m r) => Read (Quotient r m) where
  readsPrec d str = P.map (first $ (`mod` (Proxy :: Proxy m))) $ readsPrec d str

instance (Euclidean r, Show r, Reifies m r) => Show (Quotient r m) where
  showsPrec d m@(Quotient x) =
    showParen (d > 5) $ showsPrec d x . showString " mod " . showsPrec (d+1) (modulus m)

-- | WARNING: This instance should be used only when modulo @m@ is prime number.
instance Reifies m Integer => Division (Quotient Integer m) where
  recip = fromJust . recipUnit
