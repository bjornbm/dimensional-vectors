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
-- >>> let m22 = (x <:. y) |:. (z <:. z)
-- >>> let m23 = vc3 |:. v
-- >>> let m32 = transpose m23
-- >>> let m33 = vc4 |: vc3 |:. v


-- Operators
-- =========

-- | Convention:
--
--   @>@  Vector to the left of operator (mnemonic: v)
--   @<@  Vector to the right of operator (mnemonic: v)
--   @.@  Last element of vector.
--
-- The above symbols were chosen to minimize risk of conflict with common
-- operators from other libraries (based on Hoogle search).

-- Operator fixity analogous with Prelude.

--infixl 7  *|, |*, |*|, |*<, >*|
--infixl 6  |+|, |-|
infixr 5  |:, |:.

-- In these construction operators the @:@ cannot be to the left
-- so the order of characters in the operator are somewhat reversed from
-- the ideal we are forced to reverse order of characters from the ideal
-- (for consistency with other operator conventions in this module the
-- @>@ and @|@ should have been on the right side of the operator).



-- The Matrix type
-- ===============

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

-- | Construct the matrix with a single element.
  --
  -- >>> mSing x == rowMatrix (vSing x)
  -- True
  -- >>> mSing x == colMatrix (vSing x)
  -- True
  -- >>> mSing x
  -- << 2.0 m >>
mSing :: Fractional a => Quantity d a -> Mat '[ '[d] ] a
mSing x = ListMat [[x /~ siUnit]]

-- | Principled implementation of 'mSing'.
  --
  -- >>> mSing x == mSing' x
  -- True
mSing' :: Fractional a => Quantity d a -> Mat '[ '[d] ] a
mSing' = rowMatrix . vSing


-- Row wise
-- --------

type Row (ds::[k]) = '[ds]  -- For symmetry.
type ConsRow v (vs::[k]) = v ': vs  -- For symmetry.
type AppendRows vs1 vs2 = Append vs1 vs2

-- | Construct a matrix with a single row from a vector.
rowMatrix :: Vec ds a -> Mat (Row ds) a
rowMatrix (ListVec xs) = ListMat [xs]

consRow :: (VLength ds ~ Cols vs)
        => Vec ds a -> Mat vs a -> Mat (ConsRow ds vs) a
consRow (ListVec xs) (ListMat vs) = ListMat (xs:vs)

(|:) :: (VLength ds ~ Cols vs)
     => Vec ds a -> Mat vs a -> Mat (ds ': vs) a
(|:) = consRow

(|:.) :: (VLength ds1 ~ VLength ds2) => Vec ds1 a -> Vec ds2 a -> Mat '[ds1,ds2] a
v1 |:. v2 = v1 |: rowMatrix v2

appendRows :: (Cols vs1 ~ Cols vs2)
           => Mat vs1 a -> Mat vs2 a -> Mat (AppendRows vs1 vs2) a
appendRows (ListMat vs1) (ListMat vs2) = ListMat (vs1 ++ vs2)

snocRow :: (Cols vs ~ VLength ds) => Mat vs a -> Vec ds a -> Mat (Snoc vs ds) a
snocRow m v = appendRows m (rowMatrix v)


-- Column wise
-- -----------

-- TODO generic?
type family Column ds where
  Column (d ': '[]) = '[d] ': '[]
  Column (d ': ds)  = '[d] ': Column ds

-- | Create a single column matrix from the vector.
  --
  -- >>> colMatrix (vSing x) == rowMatrix (vSing x)
  -- True
  -- >>> colMatrix (2 *~ gram <: _3 <:. 32.3 *~ meter)
  -- << 2.0e-3 kg >,
  --  < 3.0 >,
  --  < 32.3 m >>
colMatrix :: Vec ds a -> Mat (Column ds) a
colMatrix (ListVec xs) = ListMat (fmap return xs)


-- TODO generic?
type family ConsCol ds vs where
  ConsCol '[d] '[v] = '[d ': v]
  ConsCol (d ': ds) (v ': vs) = (d ': v) ': ConsCol ds vs


-- | Prepend a column to a matrix.
  --
  -- >>> consCol (vSing x) (colMatrix (vSing y)) == rowMatrix (x <:. y)
  -- True
  -- >>> consCol (x <:. y) (colMatrix (z <:. x)) == (x <:. z) |:. (y <:. x)
  -- True
  -- >>> consCol (x <:. y) m23
  -- << 2.0 m, 3.0 m, 2.0, 1.0 >,
  --  < 3.0 kg, 2.0 m, 3.0 kg, 1.0 >>
consCol :: Vec ds a -> Mat vs a -> Mat (ConsCol ds vs) a
consCol (ListVec xs) (ListMat vs) = ListMat (zipWith (:) xs vs)


type AppendCols vs1 vs2 = Transpose (Append (Transpose vs1) (Transpose vs2))

-- | Append the second matrix to the first in a column-wise fashion.
  --
  -- >>> appendCols (mSing x) (mSing y) == rowMatrix (x <:. y)
  -- True
  -- >>> appendCols (colMatrix (x <:. z)) (colMatrix (y <:. x)) == (x <:. y) |:. (z <:. x)
  -- True
appendCols :: Mat vs1 a -> Mat vs2 a -> Mat (AppendCols vs1 vs2) a
appendCols (ListMat vs1) (ListMat vs2) = ListMat (zipWith (++) vs1 vs2)

-- | Principled implementation of 'appendCols'.
  --
  -- >>> appendCols m23 m23 == appendCols' m23 m23
  -- True
appendCols'
  :: (TransposeC (Append (Transpose vs1) (Transpose vs2)),
      TransposeC vs2, TransposeC vs1,
      Cols (Transpose vs1) ~ Cols (Transpose vs2)  -- Rows vs1 ~ Rows vs2
     ) => Mat vs1 a -> Mat vs2 a -> Mat (AppendCols vs1 vs2) a
appendCols' m1 m2 = transpose (appendRows (transpose m1) (transpose m2))


-- | Add a column to the right of a matrix.
  --
  -- >>> snocCol (transpose m23) vc3 == transpose (snocRow m23 vc3)
  -- True
snocCol :: Mat vs a -> Vec v a -> Mat (AppendCols vs (Column v)) a
snocCol m v = appendCols m (colMatrix v)


-- Deconstruction
-- ==============

-- Row wise
-- --------

type HeadRow  (vs::[[Dimension]]) = Head vs
type TailRows (vs::[[Dimension]]) = Tail vs

-- | Return the first row of a matrix as a vector.
  --
  -- >>> headRow (mSing x) == vSing x
  -- True
  -- >>> headRow m23 == vc3
  -- True
headRow :: Mat vs a -> Vec (HeadRow vs) a
headRow (ListMat vs) = ListVec (head vs)

-- | Drop the first row of a matrix.
  --
  -- >>> tailRows m23 == rowMatrix v
  -- True
tailRows :: Mat vs a -> Mat (TailRows vs) a
tailRows (ListMat vs) = ListMat (tail vs)


-- Column wise
-- -----------

type HeadCol  vs = HeadRow  (Transpose vs)
type TailCols vs = Transpose (TailRows (Transpose vs))

-- | Return the first column of a matrix as a vector.
  --
  -- >>> headCol (mSing x) == vSing x
  -- True
  -- >>> headCol m32 == vc3
  -- True
headCol :: Mat vs a -> Vec (HeadCol vs) a
headCol (ListMat vs) = ListVec (map head vs)

-- | Principled implementation of 'headCol'.
  --
  -- >>> headCol m32 == headCol' m32
  -- True
headCol' :: TransposeC vs => Mat vs a -> Vec (HeadCol vs) a
headCol' = headRow . transpose


-- | Drop the first column of a matrix.
  --
  -- >>> tailCols m32 == colMatrix v
  -- True
tailCols :: Mat vs a -> Mat (TailCols vs) a
tailCols (ListMat vs) = ListMat (map tail vs)

-- | Principled implementation of 'tailCols'.
  --
  -- >>> tailCols m32 == tailCols' m32
  -- True
tailCols' :: (TransposeC vs, TransposeC (Tail (Transpose vs)))
          => Mat vs a -> Mat (TailCols vs) a
tailCols' = transpose . tailRows . transpose


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


-- Transpose
-- =========

-- TODO Could have put this type in TransposeC, but since it is
-- stand-alone perhaps I could move it to ListKind instead. That
-- might, in fact, be a good approach for all HOFs!
type family Transpose (vs::[[k]]) :: [[k]] where
  Transpose '[v] = Column v
  Transpose (v ': vs) = ConsCol v (Transpose vs)

class TransposeC vs where
  -- | Transpose a matrix.
    --
    -- >>> transpose (transpose m23) == m23
    -- True
    -- >>> transpose (colMatrix v) == rowMatrix v
    -- True
    -- >>> transpose (rowMatrix v) == colMatrix v
    -- True
    -- >>> transpose (v |:. vc4) == consCol v (colMatrix vc4)
    -- True
  transpose :: Mat vs a -> Mat (Transpose vs) a

instance TransposeC '[v] where
  transpose = colMatrix . headRow  -- Principled!
instance TransposeC (v2 ': vs) => TransposeC (v1 ': v2 ': vs) where
  transpose m = consCol (headRow m) (transpose (tailRows m))  -- Principled!


-- Show
-- ====

-- | We provide a custom @Show@ instance for vectors.
  --
  -- >>> m23
  -- << 3.0 m, 2.0, 1.0 >,
  --  < 2.0 m, 3.0 kg, 1.0 >>
  -- >>> show m23
  -- "<< 3.0 m, 2.0, 1.0 >,\n < 2.0 m, 3.0 kg, 1.0 >>"
  -- >>> show (rowMatrix (2 *~ gram <: _3 <:. 32.3 *~ meter))
  -- "<< 2.0e-3 kg, 3.0, 32.3 m >>"
instance (MMapOutC Show' vs a, MMapOut Show' vs a ~ String) => Show (Mat vs a)
  where show = ("<< " ++) . (++ " >>") . intercalate " >,\n < " . map (intercalate ", ") . mMapOut Show
