> {-# OPTIONS_GHC -fglasgow-exts -fno-monomorphism-restriction #-}

> import qualified Prelude
> import Vector
> import Matrix
> import PosVel
> import Numeric.Units.Dimensional.Prelude
> import Test.QuickCheck

> mps = meter / second

> x = 4 *~ meter
> y = 1 *~ meter
> z = (-0.2) *~ meter
> v_x = 0.01 *~ mps
> v_y = (-0.1) *~ mps
> v_z = 1 *~ mps
> p = (vCons x $ vCons y $ vSing z)
> v = (vCons v_x $ vCons v_y $ vSing v_z)
> pv = CPosVel p v

> prop_linC = pv == pv' where
>   --pv = CPosVel (vCons x $ vCons y $ vSing z) (vCons v_x $ vCons v_y $ vSing v_z)
>   pv' = unlinearizeC $ linearizeC pv

> main = do
>   quickCheck prop_linC

