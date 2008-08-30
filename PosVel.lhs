
The convention in this module is that a @C@ denotes cartesian coordinates and an @S@ denotes spherical coordinates.

> {-# OPTIONS_GHC -fglasgow-exts -fno-monomorphism-restriction #-}

> module PosVel where

> import qualified Prelude
> import Vector
> import Matrix
> import ForwardAD
> import Numeric.Units.Dimensional.Prelude
> import HList
> import Fad (Dual)

Some type synonyms for convenience.

> type Vec3 d1 d2 d3 = Vec (d1 :*: d2 :*: d3 :*: HNil)
> type CPos = Vec3 DLength DLength DLength  -- x y z
> type CVel = Vec3 DVelocity DVelocity DVelocity
> type SPos = Vec3 DLength DPlaneAngle DPlaneAngle  -- r s theta
> type SVel = Vec3 DVelocity DAngularVelocity DAngularVelocity

Data type combining position and velocity into a state vector (minus epoch).

> data CPosVel a = CPosVel (CPos a) (CVel a) deriving (Eq, Show)
> data SPosVel a = SPosVel (SPos a) (SVel a) deriving (Eq, Show)


Linearizing
-----------
@linearizeC@ converts a 'CPosVel' into a function of time linearized about the original position.

> linearizeC :: Num a => CPosVel a -> (Time a -> CPos a)
> linearizeC (CPosVel p v) = \t -> p `elemAdd` (scaleVec t v)

@unlinearizeC@ converts a function of time to 'CPos' into a 'CPosVel' at time 0. I'm not super-happy with the @Floating@ constraint, which I realize I might even have to promote to @RealFloat@ at some point!

> unlinearizeC :: RealFloat a => (forall b. RealFloat b => Time b -> CPos b) -> CPosVel a
> unlinearizeC f = CPosVel (f t_0) (diffV f t_0) where t_0 = 0 *~ second

Alternative definition without the 'Floating' constraint. Note the ugliness of having to pass the function twice!

> -- unlinearizeC' :: Num a => (Time a -> CPos a) -> 
> --   (forall tag. Time (Dual tag a) -> CPos (Dual tag a)) -> CPosVel a
> -- unlinearizeC' f f' = CPosVel (f t_0) (diffV f' t_0) where t_0 = 0 *~ second

Analogous for spherical coordinates.

> linearizeS :: Num a => SPosVel a -> (Time a -> SPos a)
> linearizeS (SPosVel p v) = \t -> p `elemAdd` (scaleVec t v)

> unlinearizeS :: RealFloat a => (forall b. RealFloat b => Time b -> SPos b) -> SPosVel a
> unlinearizeS f = SPosVel (f t_0) (diffV f t_0) where t_0 = 0 *~ second


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

> c2sEphem :: RealFloat a => (forall b. RealFloat b => CPosVel b) -> SPosVel a
> c2sEphem c = unlinearizeS st
>   where
>     ct = linearizeC c -- Time -> CPos
>     st = c2s . ct     -- Time -> SPos

Converts a spherical position vector into a cartesian position vector.

> s2c :: RealFloat a => SPos a -> CPos a
> s2c s = fromHList (x .*. y .*. z .*. HNil)
>   where
>     HCons r (HCons ra (HCons dec HNil)) = toHList s
>     x = r * cos dec * cos ra
>     y = r * cos dec * sin ra
>     z = r * sin dec

> s2cEphem :: RealFloat a => (forall b. RealFloat b => SPosVel b) -> CPosVel a
> s2cEphem s = unlinearizeC ct
>   where
>     st = linearizeS s -- Time -> SPos
>     ct = s2c . st     -- Time -> CPos



Rotation matrices (cartesian)
-----------------------------
Convenience type for homogeneous 3x3 matrices.

> type Homo33 d = Mat (HCons (d :*: d :*: d :*: HNil)
>                     (HCons (d :*: d :*: d :*: HNil)
>                     (HCons (d :*: d :*: d :*: HNil)
>                     HNil)))

Rotation matrices. Rotates a vector by the given angle (analogous to rotating the coordinate system in opposite direction).

> rotX :: Floating a => PlaneAngle a -> Homo33 DOne a
> rotX a = consRow   (vCons _1 $ vCons _0      $ vSing _0)
>        $ consRow   (vCons _0 $ vCons (cos a) $ vSing (negate (sin a)))
>        $ rowMatrix (vCons _0 $ vCons (sin a) $ vSing (cos a))

> rotY :: Floating a => PlaneAngle a -> Homo33 DOne a
> rotY a = consRow   (vCons (cos a)          $ vCons _0 $ vSing (sin a))
>        $ consRow   (vCons _0               $ vCons _1 $ vSing _0)
>        $ rowMatrix (vCons (negate (sin a)) $ vCons _0 $ vSing (cos a))

> rotZ :: Floating a => PlaneAngle a -> Homo33 DOne a
> rotZ a = consRow   (vCons (cos a) $ vCons (negate (sin a)) $ vSing _0)
>        $ consRow   (vCons (sin a) $ vCons (cos a)          $ vSing _0)
>        $ rowMatrix (vCons _0      $ vCons _0               $ vSing _1)

