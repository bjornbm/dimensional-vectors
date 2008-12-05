{-# OPTIONS_GHC -fglasgow-exts #-}

module ForwardAD (diffV,diffNumV) where

import Data.HList (HMap)
import Numeric.Units.Dimensional (Dimensional (Dimensional), Quantity, Div, DOne)
import Vector (Vec (ListVec), MulD, DivD)
import Fad (dNumF, dRealFloatF)

-- | If @f@ is a function of a quantity that returns a 'Vector', then
-- @diff f@ is a function of the same type of quantity that returns
-- the first derivative of the result.
diffV :: (RealFloat a, HMap (DivD,d) ds ds')
  => (forall b. RealFloat b => Quantity d b -> Vec ds b) -> Quantity d a -> Vec ds' a
diffV f (Dimensional x) = ListVec (dRealFloatF (unvec . f . Dimensional) x)
  where
    unvec (ListVec xs) = xs

-- Other diffs with less stringent requirements on the type class to follow...
diffNumV :: (Num a, Div DOne d d', HMap (MulD, d') ds ds')
  => (forall b. Num b => Quantity d b -> Vec ds b) -> Quantity d a -> Vec ds' a
diffNumV f (Dimensional x) = ListVec (dNumF (unvec . f . Dimensional) x)
  where
    unvec (ListVec xs) = xs

