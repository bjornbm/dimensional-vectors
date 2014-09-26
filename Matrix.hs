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
import VApply
import Vector
import qualified Prelude as P

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

-- | Construct a matrix with a single row from a vector.
  --
  -- >>> rowMatrix v
  -- << 2.0 m, 3.0 kg, 1.0 >>
rowMatrix :: Vec ds a -> Mat (Row ds) a
rowMatrix (ListVec xs) = ListMat [xs]

-- | Prepend a row to a matrix.
  --
  -- >>> consRow vc3 (rowMatrix v)
  -- << 3.0 m, 2.0, 1.0 >,
  --  < 2.0 m, 3.0 kg, 1.0 >>
consRow, (|:) :: (VLength ds ~ Cols vs)
        => Vec ds a -> Mat vs a -> Mat (ConsRow ds vs) a
consRow (ListVec xs) (ListMat vs) = ListMat (xs:vs)
(|:) = consRow

-- | Create a matrix with two rows.
  --
  -- >>> vc3 |:. v == consRow vc3 (rowMatrix v)
  -- True
(|:.) :: (VLength ds1 ~ VLength ds2) => Vec ds1 a -> Vec ds2 a -> Mat '[ds1,ds2] a
v1 |:. v2 = v1 |: rowMatrix v2


-- | Append the rows of the second matrix to the first.
  --
  -- >>> appendRows (rowMatrix vc3) (rowMatrix v) == vc3 |:. v
  -- True
appendRows :: (Cols vs1 ~ Cols vs2)
           => Mat vs1 a -> Mat vs2 a -> Mat (AppendRows vs1 vs2) a
appendRows (ListMat vs1) (ListMat vs2) = ListMat (vs1 ++ vs2)

-- | Append the row at the bottom of the matrix.
  --
  -- >>> snocRow (rowMatrix vc3) v == vc3 |:. v
  -- True
  -- >>> snocRow m23 v == appendRows m23 (rowMatrix v)
  -- True
snocRow :: (Cols vs ~ VLength ds) => Mat vs a -> Vec ds a -> Mat (Snoc vs ds) a
snocRow m v = appendRows m (rowMatrix v)


-- Column wise
-- -----------

-- | Create a matrix with a single column matrix from a vector.
  --
  -- >>> colMatrix (vSing x) == rowMatrix (vSing x)
  -- True
  -- >>> colMatrix (2 *~ gram <: _3 <:. 32.3 *~ meter)
  -- << 2.0e-3 kg >,
  --  < 3.0 >,
  --  < 32.3 m >>
colMatrix :: Vec ds a -> Mat (Column ds) a
colMatrix (ListVec xs) = ListMat (fmap return xs)


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


-- | Append the colums of the second matrix to the first.
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
appendCols' :: ( TransposeC' (AppendRows (Transpose vs1) (Transpose vs2))
  , TransposeC' vs1, TransposeC' vs2
  , Cols (Transpose vs1) ~ Cols (Transpose vs2)  -- Rows vs1 ~ Rows vs2
  ) => Mat vs1 a -> Mat vs2 a -> Mat (AppendCols vs1 vs2) a
appendCols' m1 m2 = transpose' (appendRows (transpose' m1) (transpose' m2))


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
headCol' :: TransposeC' vs => Mat vs a -> Vec (HeadCol vs) a
headCol' = headRow . transpose'


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
tailCols' :: (TransposeC' vs, TransposeC' (Tail (Transpose vs)))
          => Mat vs a -> Mat (TailCols vs) a
tailCols' = transpose' . tailRows . transpose'



-- Higher order functions
-- ======================

-- Mapping to each element
-- -----------------------

type family MMap f (vs::[[Dimension]]) :: [[Dimension]] where
  MMap f '[v] = '[VMap f v]
  MMap f (v1 ': v2 ': vs) = VMap f v1 ': MMap f (v2 ': vs)

class MMapC f (vs::[[Dimension]]) a where
  mMap :: f -> Mat vs a -> Mat (MMap f vs) a

instance VMapC f v a => MMapC f '[v] a where
  mMap f = rowMatrix . vMap f . headRow

instance ( VMapC f v1 a, MMapC f (v2 ': vs) a
         , Cols (MMap f (v2 ': vs)) ~ Elements (VMap f v1)
         ) => MMapC f (v1 ': v2 ': vs) a where
  mMap f m = vMap f (headRow m) |: mMap f (tailRows m)


-- Mapping each row to a quantity
-- ------------------------------

-- | Mapping a function from a vector to a quantity over each row of
  -- a matrix.
type family MapRowQ f (vs::[[Dimension]]) :: [Dimension] where
  MapRowQ f '[v] = '[VUnaryQ f v]
  MapRowQ f (v1 ': v2 ': vs) = VUnaryQ f v1 ': MapRowQ f (v2 ': vs)

class MapRowQC f vs a where
  -- | Map a function from a vector to a quantity over each row of
    -- a matrix.
    --
    -- >>> mapRowQ Sum (vh1 |:. vh2) == vSum vh1 <:. vSum vh2
    -- True
  mapRowQ :: f -> Mat vs a -> Vec (MapRowQ f vs) a

instance (Fractional a, VUnaryQC f v a) => MapRowQC f '[v] a where
  mapRowQ f m = vSing (vUnaryQ f (headRow m))

instance (Fractional a, VUnaryQC f v1 a, MapRowQC f (v2 ': vs) a)
  => MapRowQC f (v1 ': v2 ': vs) a where
  mapRowQ f m = vUnaryQ f (headRow m) <: mapRowQ f (tailRows m)


-- | Map a function from a vector to a quantity over each column of
  -- a matrix.
  --
  -- >>> mapColQ Sum (consCol vh1 (colMatrix vh2)) == vSum vh1 <:. vSum vh2
  -- True
mapColQ :: MapRowQC f (Transpose vs) a =>
     f -> Mat vs a -> Vec (MapRowQ f (Transpose vs)) a
mapColQ f = mapRowQ f . transpose


-- Mapping each row to a vector
-- ----------------------------

-- | Mapping a function from a vector to a vector over each row of
  -- a matrix.
type family MapRowV f (vs::[[Dimension]]) :: [[Dimension]] where
  MapRowV f '[v] = '[VUnaryV f v]
  MapRowV f (v1 ': v2 ': vs) = VUnaryV f v1 ': MapRowV f (v2 ': vs)

class MapRowVC f vs a where
  -- | Map a function from a vector to a vector over each row of
    -- a matrix.
    --
    -- >>> mapRowV Id m23 == m23
    -- True
  mapRowV :: f -> Mat vs a -> Mat (MapRowV f vs) a

instance VUnaryVC f v a => MapRowVC f '[v] a where
  mapRowV f m = rowMatrix (vUnaryV f (headRow m))

instance ( VUnaryVC f v1 a, MapRowVC f (v2 ': vs) a
  , Cols (MapRowV f (v2 ': vs)) ~ Elements (VUnaryV f v1)
  ) => MapRowVC f (v1 ': v2 ': vs) a where
  mapRowV f m = vUnaryV f (headRow m) |: mapRowV f (tailRows m)


-- | Map a function from a vector to a vector over each column of
  -- a matrix.
  --
  -- >>> mapColV Id m23 == m23
  -- True
mapColV :: MapRowVC f (Transpose vs) a =>
     f -> Mat vs a -> Mat (Transpose (MapRowV f (Transpose vs))) a
mapColV f = transpose . mapRowV f . transpose


-- Zipping each element
-- --------------------

type MZipWithC f us vs a = ZipRowsWithC (Zip f) us vs a
type MZipWith  f us vs   = ZipRowsWith  (Zip f) us vs

-- | Zip each element of the matrix.
mZipWith :: MZipWithC f vs us a
          => f -> Mat vs a -> Mat us a -> Mat (MZipWith f vs us) a
mZipWith f m1 m2 = zipRowsWith (Zip f) m1 m2


-- Zipping rows to rows
-- -----------------------

type family ZipRowsWith f vs us :: [[Dimension]] where
  ZipRowsWith f '[v] '[u] = '[VBinaryV f v u]
  ZipRowsWith f (v1 ': v2 ': vs) (u1 ': u2 ': us) =
    VBinaryV f v1 u1 ': ZipRowsWith f (v2 ': vs) (u2 ': us)

class ZipRowsWithC f vs us a where
  zipRowsWith :: f -> Mat vs a -> Mat us a -> Mat (ZipRowsWith f vs us) a

instance (VBinaryVC f v u a) => ZipRowsWithC f '[v] '[u] a where
  zipRowsWith f m1 m2 = rowMatrix (vBinaryV f (headRow m1) (headRow m2))

instance (VBinaryVC f v1 u1 a, ZipRowsWithC f (v2 ': vs) (u2 ': us) a
  , Cols (ZipRowsWith f (v2 ': vs) (u2 ': us)) ~ Elements (VBinaryV f v1 u1)
  ) => ZipRowsWithC f (v1 ': v2 ': vs) (u1 ': u2 ': us) a where
  zipRowsWith f m1 m2 = vBinaryV f (headRow  m1) (headRow  m2)
                  |: zipRowsWith f (tailRows m1) (tailRows m2)


-- Mapping out
-- -----------

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
transpose (ListMat vs) = ListMat (transposeLists vs)
  where
    transposeLists :: [[a]] -> [[a]]
    transposeLists [v] = fmap return v
    transposeLists (v:vs) = zipWith (:) v (transposeLists vs)

-- | Principled implementation of 'transpose'.
  --
  -- >>> transpose m23 == transpose' m23
  -- True
  -- >>> transpose (colMatrix v) == transpose' (colMatrix v)
  -- True
  -- >>> transpose (rowMatrix v) == transpose' (rowMatrix v)
  -- True
class TransposeC' vs where
  transpose' :: Mat vs a -> Mat (Transpose vs) a
instance TransposeC' '[v] where
  transpose' = colMatrix . headRow  -- Principled!
instance TransposeC' (v2 ': vs) => TransposeC' (v1 ': v2 ': vs) where
  transpose' m = consCol (headRow m) (transpose (tailRows m))  -- Principled!



-- Unary (single matrix) operations
-- ================================

-- Scaling
-- -------

type ScaleMat d vs a = MMap (UnaryR Mul d a) vs

-- | Scale a matrix by multiplication. Each element of the matrix is
  -- multiplied by the first argument.
  --
  -- >>> scaleMat (2*~gram) (mSing $ 3 *~ meter)
  -- << 6.0e-3 m kg >>
  -- >>> scaleMat (2*~gram) (rowMatrix (_4 <:. 3 *~ meter))
  -- << 8.0e-3 kg, 6.0e-3 m kg >>
  --
  -- TODO convert to prop.
  --
  -- >>> scaleMat x m23 == mMap (UnaryR Mul x) m23
  -- True
scaleMat :: Fractional a => Quantity d a -> Mat vs a -> Mat (ScaleMat d vs a) a
scaleMat x (ListMat vs) = ListMat (map (map (P.* (x /~ siUnit))) vs)

-- | Principled implementation of 'scaleMat'.
  --
  -- >>> scaleMat x m23 == scaleMat' x m23
  -- True
scaleMat' :: MMapC (UnaryR Mul d a) vs a => Quantity d a -> Mat vs a -> Mat (ScaleMat d vs a) a
scaleMat' x = mMap (UnaryR Mul x)


-- Elementwise operations
-- ======================

-- | Elementwise addition of matrices. The matrices must have the
  -- same size and element types.
  --
  -- >>> mElemAdd m23 m23 == scaleMat _2 m23
  -- True
mElemAdd :: Num a => Mat vs a -> Mat vs a -> Mat vs a
mElemAdd (ListMat vs1) (ListMat vs2) = ListMat (zipWith (zipWith (P.+)) vs1 vs2)

-- | Principled implementation of 'mElemAdd'.
  --
  -- >>> mElemAdd m23 m23 == mElemAdd' m23 m23
  -- True
mElemAdd' :: (Num a, MZipWithC Add vs vs a, MZipWith Add vs vs ~ vs)
         => Mat vs a -> Mat vs a -> Mat vs a
mElemAdd' = mZipWith Add


-- | Elementwise subraction of matrices. The matrices must have the
  -- same size and element types.
  --
  -- >>> mElemSub m23 m23 == scaleMat _0 m23
  -- True
mElemSub :: Num a => Mat vs a -> Mat vs a -> Mat vs a
mElemSub (ListMat vs1) (ListMat vs2) = ListMat (zipWith (zipWith (P.-)) vs1 vs2)

-- | Principled implementation of 'mElemAdd'.
  --
  -- >>> mElemSub m23 m23 == mElemSub' m23 m23
  -- True
mElemSub' :: (Num a, MZipWithC Sub vs vs a, MZipWith Sub vs vs ~ vs)
         => Mat vs a -> Mat vs a -> Mat vs a
mElemSub' = mZipWith Sub


-- | Elementwise multiplication of matrices. The matrices must have the
  -- same size.
mElemMul :: Num a => Mat vs1 a -> Mat vs2 a -> Mat (MZipWith Mul vs1 vs2) a
mElemMul (ListMat vs1) (ListMat vs2) = ListMat (zipWith (zipWith (P.*)) vs1 vs2)

-- | Principled implementation of 'mElemMul'.
  --
  -- -- >>> mElemMul m23 m23 == mElemMul' m23 m23
  -- True
mElemMul' :: MZipWithC Mul vs1 vs2 a
          => Mat vs1 a -> Mat vs2 a -> Mat (MZipWith Mul vs1 vs2) a
mElemMul' = mZipWith Mul


-- | Elementwise division of matrices. The matrices must have the
  -- same size.
mElemDiv :: Fractional a
         => Mat vs1 a -> Mat vs2 a -> Mat (MZipWith Div vs1 vs2) a
mElemDiv (ListMat vs1) (ListMat vs2) = ListMat (zipWith (zipWith (P./)) vs1 vs2)

-- | Principled implementation of 'mElemDiv'.
  --
  -- -- >>> mElemDiv m23 m23 == mElemDiv' m23 m23
  -- True
mElemDiv' :: MZipWithC Div vs1 vs2 a
          => Mat vs1 a -> Mat vs2 a -> Mat (MZipWith Div vs1 vs2) a
mElemDiv' = mZipWith Div


-- Matrix/vector multiplication
-- ============================

-- TODO Better to use the type MatVec' rather than a type family?
type family MatVec vs v where
  MatVec '[v1] v2 = '[DotProduct v1 v2]
  MatVec (v1 ': vs) v2 = DotProduct v1 v2 ': MatVec vs v2

-- |
matVec :: Num a => Mat vs a -> Vec v a -> Vec (MatVec vs v) a
matVec (ListMat vs) (ListVec v) = ListVec (map (P.sum . zipWith (P.*) v) vs)

-- | Principled implementation of 'matVec'.
  --
  -- >>> matVec' (rowMatrix v) vd2 == matVec (rowMatrix v) vd2
  -- True
matVec' :: MapRowQC (VUnaryR Dot v a) vs a
        => Mat vs a -> Vec v a -> Vec (MatVec' vs v a) a
matVec' m v = mapRowQ (VUnaryR Dot v) m
type MatVec' vs v a = MapRowQ (VUnaryR Dot v a) vs


type VecMat v vs = MatVec (Transpose vs) v

-- |
vecMat :: Num a => Vec v a -> Mat vs a -> Vec (VecMat v vs) a
vecMat v m = matVec (transpose m) v


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
