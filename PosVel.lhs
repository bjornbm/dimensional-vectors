
The convention in this module is that a @C@ denotes cartesian coordinates and an @S@ denotes spherical coordinates.

> {-# OPTIONS_GHC -fglasgow-exts #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> module PosVel where

> import qualified Prelude
> import Vector
> import Matrix
> import Numeric.Units.Dimensional.Prelude
> import Numeric.Units.Dimensional (Dimensional (Dimensional))
> import Data.HList
> import ForwardAD (applyLinear)


Type synonyms for clearer documentation.

> type DRadius  = DLength; type Radius  = Length
> type DZenith  = DPlaneAngle ; type Zenith  = Angle
> type DAzimuth = DPlaneAngle ; type Azimuth = Angle


Some type synonyms for convenience.

> type Vec3 d1 d2 d3 = Vec (d1 :*: d2 :*: d3 :*: HNil)
> type CPos = Vec3 DLength DLength DLength  -- ^ x y z
> type CVel = Vec3 DVelocity DVelocity DVelocity
> type SPos = Vec3 DRadius DZenith DAzimuth
> type SVel = Vec3 DVelocity DAngularVelocity DAngularVelocity

Data type combining position and velocity into a state vector (minus epoch).

> type CPosVel a = (CPos a, CVel a)
> type SPosVel a = (SPos a, SVel a)


Querying
--------

Cartesian position.

> x :: CPos a -> Length a
> x = vElemAt zero
> y :: CPos a -> Length a
> y = vElemAt pos1
> z :: CPos a -> Length a
> z = vElemAt pos2

Spherical position.

> radius :: SPos a -> Radius a
> radius = vElemAt zero

> zenith :: SPos a -> Zenith a
> zenith = vElemAt pos1
> colatitude = zenith
> polarAngle = zenith
> latitude s = pi / _2 - colatitude s
> declination = latitude
> elevation   = latitude

> azimuth :: SPos a -> Azimuth a
> azimuth = vElemAt pos2
> longitude      = azimuth
> rightAscension = azimuth
> hourAngle      = azimuth


Converting
----------
Converts a cartesian position vector into a spherical position vector.

> c2s :: RealFloat a => CPos a -> SPos a
> c2s c = fromTuple (r, zen, az)
>   where
>     (x, y, z) = toTuple c
>     r   = vNorm c  -- sqrt (x^pos2 + y^pos2 + z^pos2)
>     zen = if r == 0 *~ meter then _0 else acos (z / r)
>     az  = atan2 y x

> c2sEphem :: RealFloat a => CPosVel a -> SPosVel a
> c2sEphem = applyLinear c2s -- unlinearize (c2s . linearize c :: RealFloat b => Time b -> SPos b)

Converts a spherical position vector into a cartesian position vector.

> s2c :: Floating a => SPos a -> CPos a
> s2c s = fromTuple (x, y, z)
>   where
>     (r, zen, az) = toTuple s
>     x = r * sin zen * cos az
>     y = r * sin zen * sin az
>     z = r * cos zen

> s2cEphem :: RealFloat a => SPosVel a -> CPosVel a
> s2cEphem = applyLinear s2c -- unlinearize (s2c . linearize s :: RealFloat b => Time b -> CPos b)


