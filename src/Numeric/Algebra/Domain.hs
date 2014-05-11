{-# LANGUAGE ConstraintKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, UndecidableInstances                #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Numeric.Algebra.Domain where
import           Data.Ratio
import           GHC.Real                  ((^^))
import           Numeric.Algebra.Division  (Division (..))
import           Numeric.Algebra.Instances ()
import           Numeric.Ring.Class
import           Prelude                   (Integer)
import           Prelude                   (Integral)
import           Prelude                   (flip)
import qualified Prelude                   as P

-- | Integral domain. This should satisfy following property:
--
--   prop> n * m == 0 ==> n == 0 || m == 0
class Ring r => Domain r
instance Domain Integer
instance (Integral r, Domain r) => Domain (Ratio r)

instance (Domain r, Integral r) => Division (Ratio r) where
  recip = P.recip
  (/)   = (P./)
  (\\)  = flip (P./)
  (^)   = (^^)

