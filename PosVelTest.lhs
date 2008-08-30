> {-# OPTIONS_GHC -fglasgow-exts -fno-monomorphism-restriction #-}

> import qualified Prelude
> import Vector
> import Matrix
> import PosVel
> import Numeric.Units.Dimensional.Prelude
> import Test.QuickCheck

> mps = meter / second

> x = 42164 *~ kilo meter
> y = 1 *~ meter
> z = (-0.2) *~ meter
> v_x = 0.01 *~ mps
> v_y = 3075 *~ mps
> v_z = 1 *~ mps
> p = (vCons x $ vCons y $ vSing z)
> v = (vCons v_x $ vCons v_y $ vSing v_z)
> pvc = CPosVel p v
> pvs = c2sEphem pvc

> prop_linC = pvc == pv' where
>   pv' = unlinearizeC $ linearizeC pvc

> prop_linS = pvs == pv' where
>   pv' = unlinearizeS $ linearizeS pvs

> main = do
>   quickCheck prop_linC
>   quickCheck prop_linS
>   print p
>   print $ s2c $ c2s p
>   print $ c2s p
>   print $ c2s $ s2c $ c2s p
>   print pvc
>   print $ s2cEphem $ c2sEphem pvc
>   print pvs
>   print $ c2sEphem $ s2cEphem pvs

