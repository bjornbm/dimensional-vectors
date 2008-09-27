
The convention in this module is that a @C@ denotes cartesian coordinates and an @S@ denotes spherical coordinates.

> {-# OPTIONS_GHC -fglasgow-exts -fno-monomorphism-restriction #-}

> module PosVel where

> import qualified Prelude
> import Vector
> import Matrix
> import ForwardAD
> import Numeric.Units.Dimensional.Prelude
> import Numeric.Units.Dimensional (Dimensional (Dimensional))
> import Data.HList
> import Fad (Dual)

Some type synonyms for convenience.

> type Vec3 d1 d2 d3 = Vec (d1 :*: d2 :*: d3 :*: HNil)
> type CPos = Vec3 DLength DLength DLength  -- x y z
> type CVel = Vec3 DVelocity DVelocity DVelocity
> type SPos = Vec3 DLength DPlaneAngle DPlaneAngle  -- r s theta
> type SVel = Vec3 DVelocity DAngularVelocity DAngularVelocity

Data type combining position and velocity into a state vector (minus epoch).

> type CPosVel a = (CPos a, CVel a)
> type SPosVel a = (SPos a, SVel a)


Linearizing
-----------
@linearize@ converts a pair of a vector and its derivative into a function of time linearized about the original vector at @t=0@. In order for the function to be differetiable the numeric representation of the function cannot be limited to that of the inputs. Therefore we need to use 'fromRealFrac' to coerce the types. Hopefully the compiler is clever about optimizing these away when going to/from the same type.

> linearize :: forall a b d ds ds'. (Real a, Fractional b, HMap (MulD,d) ds' ds)
>           => (Vec ds a, Vec ds' a) -> (Quantity d b -> Vec ds b)
> linearize (p, v) = \t -> p' `elemAdd` (scaleVec t v') 
>   where
>     p' = vMap fromRealFrac p :: Vec ds b
>     v' = vMap fromRealFrac v :: Vec ds' b
>     fromRealFrac = Prelude.fromRational . Prelude.toRational

@unlinearize@ converts a function of @x@ to a vector into a pair of the vector and its derivative at @x=0@. I'm not super-happy with the @RealFloat@ constraint but it is necessary for deriving.

> unlinearize :: ( RealFloat a, HMap (DivD,d) ds ds')
>             => (forall b. RealFloat b => Quantity d b -> Vec ds b) -> (Vec ds a, Vec ds' a)
> unlinearize f = (f t_0, diffV f t_0) where t_0 = Dimensional 0


Converting
----------
Converts a cartesian position vector into a spherical position vector.

> c2s :: RealFloat a => CPos a -> SPos a
> c2s c = fromHList (r .*. ra .*. dec .*. HNil)
>   where
>     HCons x (HCons y (HCons z HNil)) = toHList c
>     r   = sqrt (x^pos2 + y^pos2 + z^pos2)
>     ra  = atan2 y x
>     dec = if r == 0 *~ meter then _0 else asin (z / r)

> c2sEphem :: RealFloat a => CPosVel a -> SPosVel a
> c2sEphem c = unlinearize (c2s . linearize c :: RealFloat b => Time b -> SPos b)

Converts a spherical position vector into a cartesian position vector.

> s2c :: Floating a => SPos a -> CPos a
> s2c s = fromHList (x .*. y .*. z .*. HNil)
>   where
>     HCons r (HCons ra (HCons dec HNil)) = toHList s
>     x = r * cos dec * cos ra
>     y = r * cos dec * sin ra
>     z = r * sin dec

> s2cEphem :: RealFloat a => SPosVel a -> CPosVel a
> s2cEphem s = unlinearize (s2c . linearize s :: RealFloat b => Time b -> CPos b)


