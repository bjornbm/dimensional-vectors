{-# LANGUAGE NoMonomorphismRestriction#-}


module KalmanExample where

import qualified Prelude
import Data.HList
import Numeric.Units.Dimensional.Prelude
import Vector
import Matrix hiding (x,y,z)

-- Types.
type State = Vec (HCons DLength (HCons DVelocity HNil))
type Obs   = Vec (HCons DLength HNil)
type Control = Vec (HCons DAcceleration HNil)


-- The process.
f dt = fromRowHLists $  (_1                 .*. dt .*. HNil)
                    .*. (0 *~ second ^ neg1 .*. _1 .*. HNil)
                    .*. HNil

g dt = colMatrix $ fromHList $  dt ^ pos2 / _2
                            .*. dt
                            .*. HNil

--q dt = (sigma_a ^ pos2) `scaleMat` gg where gg = colMatrix (g dt) `matMat` rowMatrix (g dt)

-- Updating.
x :: Fractional a => State a -> Control a -> Time a -> State a
x x_old a dt = (f dt `matVec` x_old) `elemAdd` (g dt `matVec` a)

p p_old dt = (f dt `matMat` p_old) `matMat` transpose (f dt)

-- Observation.
z :: Num a => State a -> Obs a -> Obs a
z x v = matVec h x `elemAdd` v
h = rowMatrix $ fromHList $ _1 .*. 0 *~ second .*. HNil

r = rowMatrix $ vSing $ sigma_z ^ pos2

-- Initial state and covariance.
x_0 :: Fractional a => State a
x_0 = fromHList $ 0 *~ meter
              .*. 0 *~ (meter / second)
              .*. HNil
p_0 = fromRowHLists $ (b *~ meter ^ pos2            .*. 0 *~ (meter ^ pos2 / second)        .*. HNil)
                  .*. (0 *~ (meter ^ pos2 / second) .*. b *~ (meter ^ pos2 / second ^ pos2) .*. HNil)
                  .*. HNil where b = 0

-- Some simulation test values.
dt_1 = 1.0 *~ second  -- Time step.
a_1 = vSing $ 0.01 *~ (meter / second ^ pos2)  -- Acceleration (noisy but not noise!).
sigma_a = 0.01 *~ (meter / second ^ pos2)
v_1 = vSing $ 0.01 *~ meter  -- Measurement noise.
sigma_z = 0.001 *~ meter

-- Some updates...
x_1 = x x_0 a_1 dt_1
p_1 = p p_0 dt_1
z_1 = z x_1 v_1

y_1 = z_1 `elemSub` matVec h x_1  -- Innovation.
s_1 = h `matMat` p_0 `matMat` transpose h `mElemAdd` r

