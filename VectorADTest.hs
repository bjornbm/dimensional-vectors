{-# LANGUAGE NoMonomorphismRestriction#-}
{-# LANGUAGE TypeOperators #-}

import qualified Prelude
import Data.HList
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (gee)
import Vector
import VectorAD
import PosVel
import Test.QuickCheck

myFun :: Fractional a => Time a -> Vec (DLength :*: DOne :*: DOne :*: HNil) a
myFun t = vCons (1 *~ gee * t ^ pos2) $ vCons (10 *~ hertz * t) $ vSing _3

-- Helix example from beta
a = 2 *~ meter; b = 4.2 *~ gram
helix   t = vCons (a * cos t) $ vCons (a * sin t) $ vSing (b * t)
helix'  t = vCons (negate a * sin t) $ vCons (a * cos t) $ vSing b
helix'' t = vCons (negate a * cos t) $ vCons (negate a * sin t) $ vSing (0 *~ gram)

prop_helix' t = helix' t' == diffV helix t' 
  where t' = t *~ radian :: Dimensionless Double
prop_helix'' t = helix'' t' == diffV (diffV helix) t' 
  where t' = t *~ radian :: Dimensionless Double
prop_helix''2 t = helix'' t' == diffV helix' t' 
  where t' = t *~ radian :: Dimensionless Double

-- Velocities
x0 = 2 *~ meter; x' = 1 *~ (meter / second); x'' = 0.01 *~ (meter / second ^ pos2)
y0 = 2 *~ gram; y' = 3 *~ (gram / second)
pos t = vCons (x0 + x' * t + x'' * t ^ pos2) $ vSing (y0 + y' * t)
vel t = vCons (x' + _2 * x'' * t) $ vSing y'
acc t = vCons (_2 * x'') $ vSing (0 *~ (gram / second ^ pos2))

prop_vel t = vel t' == diffV pos t' 
  where t' = t *~ second :: Time Double
prop_acc t = acc t' == diffV (diffV pos) t' 
  where t' = t *~ second :: Time Double
prop_acc2 t = acc t' == diffV vel t' 
  where t' = t *~ second :: Time Double

-- If the function being linearized has no dependency on time the
-- results from applyLinear and applyLinearAt are identical.
-- (This test was written as much to verify that applyLinearAt can be
-- satisfactorily type checked.)
prop_applyLinearAt t y z vx vy vz = applyLinear c2s (p,v) == applyLinearAt (const c2s) t' (p,v)
  where
    t' = t*~second :: Time Double
    p = fromTuple (1*~meter,y*~meter,z*~meter)
    v = fromTuple (mps vx, mps vy, mps vz)
    mps = (*~(meter/second)) :: Double -> Velocity Double


main = do
  quickCheck prop_applyLinearAt
  quickCheck prop_helix'
  quickCheck prop_helix''
  quickCheck prop_helix''2
  quickCheck prop_vel
  quickCheck prop_acc
  quickCheck prop_acc2

