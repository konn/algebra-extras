{-# LANGUAGE ConstraintKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, UndecidableInstances                #-}
module Numeric.Algebra.Domain where
import Data.Ratio
import Numeric.Ring.Class

-- | Integral domain. This should satisfy following property:
--
--   prop> n * m == 0 ==> n == 0 || m == 0
class Ring r => Domain r
