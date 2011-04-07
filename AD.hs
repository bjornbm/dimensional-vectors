-- This module provides forwards automatic differentiation for quantities.

{-# OPTIONS_GHC -fglasgow-exts #-}

module AD (diff, Lift, lift) where

import Numeric.Units.Dimensional (Dimensional (Dimensional), Quantity, Div)
import Numeric.AD (AD, Mode)
import qualified Numeric.AD (diff, lift)

-- | Unwrap a Dimensionals numeric representation.
undim :: Dimensional v d a -> a
undim (Dimensional a) = a

diff :: (Num a, Div d2 d1 d3)
     => (forall tag. Mode tag => Quantity d1 (AD tag a) -> Quantity d2 (AD tag a))
     -> Quantity d1 a -> Quantity d3 a
diff f = Dimensional . Numeric.AD.diff (undim . f . Dimensional) . undim


class Lift w where lift :: (Num a, Mode t) => w a -> w (t a)
instance Lift (Dimensional v d)
  --where lift (Dimensional x) = Dimensional (Numeric.AD.lift x)
  where lift = Dimensional . Numeric.AD.lift . undim
