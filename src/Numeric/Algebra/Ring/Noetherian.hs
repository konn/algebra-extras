{-# LANGUAGE DataKinds, ExistentialQuantification, FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances                                           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Numeric.Algebra.Ring.Noetherian (Noetherian) where
import qualified Data.Complex              as C
import           Data.Function
import           Data.Ord
import           Data.Ratio
import           Numeric.Algebra
import qualified Numeric.Algebra.Complex   as NA
import           Numeric.Algebra.Instances
import           Prelude                   hiding (negate, subtract, (*), (+),
                                            (-))
import qualified Prelude                   as P
-- | Noetherian ring (i.e. every ideal is finitely generated).
class (Commutative r, Ring r) => Noetherian r where

instance Noetherian Int where

instance Noetherian Integer where

instance (Commutative (NA.Complex r), Ring (NA.Complex r)) => Noetherian (NA.Complex r) where
instance (Commutative (C.Complex r), Ring (C.Complex r)) => Noetherian (C.Complex r) where
instance Integral n => Noetherian (Ratio n)

