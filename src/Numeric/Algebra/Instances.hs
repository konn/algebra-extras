{-# LANGUAGE DataKinds, ExistentialQuantification, FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances                                           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Numeric.Algebra.Instances () where
import           Data.Function
import           Data.Ratio
import           Numeric.Algebra
import qualified Numeric.Algebra.Complex as NA
import           Numeric.Decidable.Units
import           Numeric.Decidable.Zero
import           Prelude                 hiding (negate, subtract, (*), (+),
                                          (-))
import qualified Prelude                 as P

instance Integral n => InvolutiveMultiplication (Ratio n) where
  adjoint = id
instance Integral n => InvolutiveSemiring (Ratio n)

instance Integral n => TriviallyInvolutive (Ratio n)

instance (Integral r, DecidableZero r) => DecidableZero (Ratio r) where
  isZero r = isZero (numerator r)

instance (Integral r, DecidableUnits r) => DecidableUnits (Ratio r)  where
  recipUnit r
    | isUnit (numerator r) = Just $ P.recip r
    | otherwise = Nothing

instance (P.Num n) => P.Num (NA.Complex n) where
  abs = error "unimplemented"
  signum = error "unimplemented"
  fromInteger n = NA.Complex (P.fromInteger n) 0
  negate (NA.Complex x y) = NA.Complex (P.negate x) (P.negate y)
  NA.Complex x y + NA.Complex z w = NA.Complex (x P.+ y) (z P.+ w)
  NA.Complex x y * NA.Complex z w = NA.Complex (x P.* z P.- y P.* w) (x P.* w P.+ y P.* z)

instance Integral n => Commutative (Ratio n)

instance Integral n => Ring (Ratio n) where
  fromInteger = P.fromInteger
instance Integral n => Rig (Ratio n) where
  fromNatural = P.fromInteger . toInteger
instance Integral n => Monoidal (Ratio n) where
  zero = 0
instance Integral n => LeftModule Natural (Ratio n) where
  n .* r = P.sum $ replicate (fromIntegral n) r

instance Integral n => RightModule Natural (Ratio n) where
  (*.) = flip (.*)

instance Integral n => Unital (Ratio n) where
  one = 1
  pow r n = r ^^ toInteger n
instance Integral n => Group (Ratio n) where
  negate = P.negate
  times n r = toInteger n .* r
  (-) = (P.-)
  subtract = P.subtract
instance Integral n => LeftModule Integer (Ratio n) where
  n .* r = fromIntegral n P.* r
instance Integral n => RightModule Integer (Ratio n) where
  r *. n = r P.* fromIntegral n
instance Integral n => Semiring (Ratio n)
instance Integral n => Additive (Ratio n) where
  (+) = (P.+)
  sinnum1p n r = fromIntegral (n P.+ 1) P.* r
instance Integral n => Abelian (Ratio n)
instance Integral n => Multiplicative (Ratio n) where
  (*) = (P.*)
  pow1p r n = r ^^ (n P.+ 1)

instance Integral n => LeftModule (Ratio n) (Ratio n) where
    (.*) = (*)

instance Integral n => RightModule (Ratio n) (Ratio n) where
    (*.) = (*)
