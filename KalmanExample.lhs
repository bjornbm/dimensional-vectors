> {-# OPTIONS_GHC -fno-monomorphism-restriction #-}

> module KalmanExample where

> import qualified Prelude
> import HList
> import Numeric.Units.Dimensional.Prelude
> import Vector
> import Matrix

Types.

> type State = Vec (HCons DLength (HCons DVelocity HNil))


The process.

> f dt = fromRowHLists $ (_1                 .*. dt .*. HNil)
>                    .*. (0 *~ second ^ neg1 .*. _1 .*. HNil)
>                    .*. HNil

> g dt = fromHList $ dt ^ pos2 / _2
>                .*. dt
>                .*. HNil

> --q dt = (sigma_a ^ pos2) `scaleMat` gg where gg = colMatrix (g dt) `matMat` rowMatrix (g dt)

Updating.

> x :: Fractional a => State a -> Acceleration a -> Time a -> State a
> x x_old a dt = (f dt `matVec` x_old) `elemAdd` (a `scaleVec` g dt)

> p p_old dt = (f dt `matMat` p_old) `matMat` transpose (f dt)

Observation.

> z :: Num a => State a -> Length a -> Length a
> z x v = dotProduct h x + v
> h = fromHList $ _1 .*. 0 *~ second .*. HNil

> r = sigma_z

Initial state and covariance.

> x_0 :: Fractional a => State a
> x_0 = fromHList $ 0 *~ meter
>               .*. 0 *~ (meter / second)
>               .*. HNil
> p_0 = fromRowHLists $ (b *~ meter ^ pos2            .*. 0 *~ (meter ^ pos2 / second)        .*. HNil)
>                   .*. (0 *~ (meter ^ pos2 / second) .*. b *~ (meter ^ pos2 / second ^ pos2) .*. HNil)
>                   .*. HNil where b = 0

Some simulation test values.

> dt_1 = 1.0 *~ second  -- Time step.
> a_1 = 0.01 *~ (meter / second ^ pos2)  -- Acceleration (noisy but not noise!).
> sigma_a = 0.01 *~ (meter / second ^ pos2)
> v_1 = 0.01 *~ meter  -- Measurement noise.
> sigma_z = 0.001 *~ meter

Some updates...

> x_1 = x x_0 a_1 dt_1
> p_1 = p p_0 dt_1
> z_1 = z x_1 v_1

> y_1 = z_1 - dotProduct h x_1 
