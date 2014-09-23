{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Matrix where

import Data.List (intercalate)
import GHC.TypeLits hiding (type (*))
import Numeric.Units.Dimensional.DK.Prelude
import Apply
import ListKind
import Vector
import qualified Prelude

-- $setup
-- >>> let x = 2 *~ meter :: Length Double
-- >>> let y = 3 *~ kilo gram :: Mass Double
-- >>> let z = _1
-- >>> let v = x <: y <:. z
-- >>> let vh1 = y <: y <:. y
-- >>> let vh2 = x <: 1 *~ meter <:. 4 *~ meter
-- >>> let vd2 = y <: x <:. x*y
-- >>> let vc3 = 3.0 *~ meter <: (2 *~ one) <:. (1 *~ one)
-- >>> let vc4 = 1 *~ (meter / second) <: 2 *~ hertz <:. 3 *~ hertz
-- >>> let f = (*) :: Length Double -> Mass Double -> FirstMassMoment Double
-- >>> let m = vc3 |:. v

infixr 5  |:, |:.

-- | A matrix is a list of rows (which in turn are lists). The matrix construction
-- functions available (i.e. 'consRow') guarantee that matrices are well-formed
-- (each row has the same number of elements). That a matrix is a list of rows as
-- opposed to a list of columns is an implementation detail that we try to not leak
-- through the API. Ultimately, however, that choice will be visible through the
-- type signatures of matrices.
data Mat (vs::[[Dimension]]) a = ListMat [[a]] deriving Eq

type Rows (vs::[[Dimension]]) = Elements vs
type Cols (vs::[[Dimension]]) = VLength (HeadRow vs)

type family RectangularC vs  where
  RectangularC '[v]      = v ~ v
  RectangularC (v ': vs) = (RectangularC vs, VLength (HeadRow vs) ~ Cols (TailRows vs))

type SquareC vs = (RectangularC vs, Rows vs ~ Cols vs)


-- Matrix construction
-- ===================

rowMatrix :: Vec ds a -> Mat '[ds] a
rowMatrix (ListVec xs) = ListMat [xs]

consRow :: (VLength ds ~ Cols vs)
        => Vec ds a -> Mat vs a -> Mat (ds ': vs) a
consRow (ListVec xs) (ListMat vs) = ListMat (xs:vs)

(|:) :: (VLength ds ~ Cols vs)
     => Vec ds a -> Mat vs a -> Mat (ds ': vs) a
(|:) = consRow

(|:.) :: (VLength ds1 ~ VLength ds2) => Vec ds1 a -> Vec ds2 a -> Mat '[ds1,ds2] a
v1 |:. v2 = v1 |: rowMatrix v2

appendRows :: (Cols vs1 ~ Cols vs2)
           => Mat vs1 a -> Mat vs2 a -> Mat (Append vs1 vs2) a
appendRows (ListMat vs1) (ListMat vs2) = ListMat (vs1 ++ vs2)

snocRow :: (Cols vs ~ VLength ds) => Mat vs a -> Vec ds a -> Mat (Snoc vs ds) a
snocRow m v = appendRows m (rowMatrix v)


-- Deconstruction
-- ==============

type HeadRow  (vs::[[Dimension]]) = Head vs
type TailRows (vs::[[Dimension]]) = Tail vs

headRow :: Mat vs a -> Vec (HeadRow vs) a
headRow (ListMat vs) = ListVec (head vs)

tailRows :: Mat vs a -> Mat (TailRows vs) a
tailRows (ListMat vs) = ListMat (tail vs)


-- Higher order functions
-- ======================

class MMapOutC f (vs::[[Dimension]]) a where
  type MMapOut f vs a
  mMapOut :: f -> Mat vs a -> [[MMapOut f vs a]]  -- Or per vector?

instance VMapOutC f ds a => MMapOutC f '[ds] a where
  type MMapOut f '[ds] a = VMapOut f ds a
  mMapOut f m = [vMapOut f (headRow m)]

instance (MMapOutC f (v2 ': vs) a, VMapOutC f v1 a, MMapOut f (v2 ': vs) a ~ VMapOut f v1 a)
  => MMapOutC f (v1 ': v2 ': vs) a where
  type MMapOut f (v1 ': v2 ': vs) a = VMapOut f v1 a
  mMapOut f m = vMapOut f (headRow m) : mMapOut f (tailRows m)




-- Show
-- ====

-- | We provide a custom @Show@ instance for vectors.
  --
  -- >>> show m
  -- "<< 3.0 m, 2.0, 1.0 >,\n < 2.0 m, 3.0 kg, 1.0 >>"
  -- >>> show (rowMatrix (2 *~ gram <: _3 <:. 32.3 *~ meter))
  -- "<< 2.0e-3 kg, 3.0, 32.3 m >>"
instance (MMapOutC Show' vs a, MMapOut Show' vs a ~ String) => Show (Mat vs a)
  where show = ("<< " ++) . (++ " >>") . intercalate " >,\n < " . map (intercalate ", ") . mMapOut Show
