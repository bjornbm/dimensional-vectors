{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Convention:
--
--   @>@  Vector to the left of operator (mnemonic: v)
--   @<@  Vector to the right of operator (mnemonic: v)
--   @|@  Matrix to side of operator
--   @.@  Last element of vector/matrix.
--
-- The above symbols were chosen to minimize risk of conflict with common
-- operators from other libraries (based on Hoogle search).

module Numeric.Units.Dimensional.LinearAlgebra.Operators where

import Numeric.Units.Dimensional.LinearAlgebra.Vector
import Numeric.Units.Dimensional.LinearAlgebra.Matrix
import Data.HList
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra.HListExtras (HNatNumType)
import qualified Prelude


-- Operator fixity analogous with Prelude.
infixl 9  >!!
infixl 7  *<, >*, >/, >.<, *|, |*, |*|, |*<, >*|
infixl 6  >+<, >-<, |+|, |-|
infixr 5  <:, <:., |:, |:.

-- These in these construction operators the @:@ cannot be to the left
-- so the order of characters in the operator are somewhat reversed from
-- the ideal we are forced to reverse order of characters from the ideal
-- (for consistency with other operator conventions in this module the
-- @>@ and @|@ should have been on the right side of the operator).

(<:) = vCons
x <:. y = x <: vSing y
(|:)  :: Wellformed (v:*:vs) => Vec v a -> Mat vs a -> Mat (v:*:vs) a
(|:)  = consRow
v1 |:. v2 = v1 |: rowMatrix v2

-- | Vector element querying.
(>!!) :: (HNatNumType n' n, HLookupByHNat n' ds d)
        => Vec ds a -> n -> Quantity d a
v >!! n = vElemAt n v

-- Vectors
(>+<), (>-<) :: Num a => Vec v a -> Vec v a -> Vec v a
(>+<) = elemAdd
(>-<) = elemSub
(*<)  :: (HMap (MulD, d) v1 v2, Num a) => Quantity d a -> Vec v1 a -> Vec v2 a
(*<)  = scaleVec
(>*)  :: (HMap (MulD, d) v1 v2, Num a) => Vec v1 a -> Quantity d a -> Vec v2 a
(>*)  = flip scaleVec
(>/)  :: (Div DOne d d', HMap (MulD, d') v1 v2, Fractional a)
      => Vec v1 a -> Quantity d a -> Vec v2 a
v >/ x = v >* (_1 / x)
(>.<) :: (DotProduct v1 v2 d, Num a) => Vec v1 a -> Vec v2 a -> Quantity d a
(>.<) = dotProduct

-- Matrices
(|+|), (|-|) :: Num a => Mat m a -> Mat m a -> Mat m a
(|+|) = mElemAdd
(|-|) = mElemSub
(|*|) :: (MatrixMatrix m1 m2 m3, Num a) => Mat m1 a -> Mat m2 a -> Mat m3 a
(|*|) = matMat
(*|)  :: (HMap (ScaleV, d) vs1 vs2, Num a) => Quantity d a -> Mat vs1 a -> Mat vs2 a
(*|)  = scaleMat
(|*)  :: (HMap (ScaleV, d) vs1 vs2, Num a) => Mat vs1 a -> Quantity d a -> Mat vs2 a
(|*)  = flip scaleMat
(|*<) :: (MatrixVector m v1 v2, Num a) => Mat m a -> Vec v1 a -> Vec v2 a
(|*<) = matVec
(>*|) :: (Transpose m m', MatrixVector m' v v', Num a) => Vec v a -> Mat m a -> Vec v' a
(>*|) v m = transpose m |*< v   -- vecMat v m
