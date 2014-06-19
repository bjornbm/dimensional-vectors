{-
A representation of vectors where elements are quantities that may
have mixed physical dimensions. The goal is to check at compile
time that for any given operation the involved vectors have compatible
dimensions and that their elements have compatible physical dimensions.

One could argue that in most cases it makes little sense to have
quantities of different physical dimensions in any given vector/matrix,
and in general I agree. (Indeed, if we were to allow only "homogeneous"
vectors our (type-level) implementation could be much simplified.
Type-level HLists would not be necessary, only a single type parameter
for the physical dimension together with a 'PosType' for the length.)
However, linear algebra applications like kalman filtering and
weighted least squares estimation use heterogeneous state vectors
and covariance matrices.

In our initial implementation we use an inefficient internal
represenation of vectors based on plain lists. The idea is that
changing the internal representation to something more efficient
(e.g.  GSLHaskell) will be transparent once all the type trickery
has been worked out.

NOTE: This library allows construction of vectors and matrices with
no elements, e.g. using vTail. It could be argued that such vectors
and matrices should be disallowed, but the price would be more
complex type class instances. In practice, due to the static checking
for all operations I believe this is a non-issue and there is really
nothing dangerous or misleading one could do with the empty
vectors/matrices.
-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Units.Dimensional.LinearAlgebra.Vector
  {- ( Vec
  , vEmpty, vHead, vTail, vCons
  , fromHList, toHList
  ) -} where

import Data.List (intercalate)
import Data.HList
import Numeric.Units.Dimensional.LinearAlgebra.HListExtras
import Numeric.NumType.DK (toNum, NumType (Pos2))
import Numeric.Units.Dimensional.DK.Prelude
import Numeric.Units.Dimensional.DK (Dimensional (Dimensional), Quantity, (*))
import qualified Orthogonals as O
import qualified Prelude as P




-- Vector data type
-- ================

-- | A vector representation based on regular lists. The intent is that
-- the @ds@ type variable should be an HList of physical dimensions
-- corresponding to the elements of the list @[a]@. This will be an
-- abstract data type with constructor functions guaranteeing this
-- invariant.
data Vec ds a = ListVec [a] deriving (Eq)

-- Showing
-- -------
-- We implement a custom @Show@ instance.

data ShowElem = ShowElem
instance Show a => Apply ShowElem a String where apply _ = show

instance (VHList (Vec ds a) l, HMapOut ShowElem l String) => Show (Vec ds a)
  where show = (\s -> "< " ++ s ++ " >")
             . intercalate ", "
             . hMapOut ShowElem
             . toHList


{-
Vector Construction and Deconstruction
======================================
Vectors can be primitively constructed using 'vCons' and 'vSing' and
deconstructed using 'vHead' and 'vTail'. We also provide type classes
for converting to/from HLists and tuples.
-}

-- | Create a singleton vector.
vSing :: Quantity d a -> Vec (HSing d) a
vSing x = ListVec [x /~ siUnit]

-- | Prepend an element to the vector.
vCons :: Quantity d a -> Vec ds a -> Vec (d:*:ds) a
vCons x (ListVec xs) = ListVec ((x /~ siUnit):xs)

-- | Return the first element of the vector.
vHead :: Vec (d:*:ds) a -> Quantity d a
vHead (ListVec xs) = head xs *~ siUnit

-- | Drop the first element of the vector.
vTail :: Vec (d:*:ds) a -> Vec ds a  -- Can create empty vector.
vTail (ListVec xs) = ListVec (tail xs)


-- Convert to/from HLists
-- ----------------------
-- |This class allows converting between vectors and the equivalent HLists.
-- of empty vectors.
class VHList v l | v -> l, l -> v where
    toHList   :: v -> l
    fromHList :: l -> v

instance VHList (Vec (HSing d) a) (HSing (Quantity d a))
  where
    toHList v = HCons (vHead v) HNil
    fromHList (HCons x HNil) = vSing x

instance VHList (Vec ds a) l => VHList (Vec (d:*:ds) a) (Quantity d a:*:l)
  where
    toHList v = HCons (vHead v) (toHList $ vTail v)
    fromHList (HCons x l) = vCons x (fromHList l)

-- Iteration
-- ---------
-- | Iterate a function over the elements of a vector. This can
-- be considered a form of uncurrying so the function operates
-- on a vector.
class VIterate ds a f b | ds f -> b
  where vIterate :: (Quantity d a -> f) -> Vec (d:*:ds) a -> b a
instance VIterate (HNil) a (b a) b where vIterate f = f . vHead
instance VIterate ds a f b => VIterate (d:*:ds) a (Quantity d a -> f) b
  where vIterate f v = f (vHead v) `vIterate` vTail v

-- Convert to/from Tuples
-- ----------------------
-- | Convert to/from tuple representation. This is primarily to allow taking
-- advantage of the syntactic sugar tuples enjoy.
class VTuple v t | v -> t, t -> v where
  toTuple   :: v -> t
  fromTuple :: t -> v

{-
We can brute force the instances out to a reasonable degree. Presumably
syntactic sugar loses its value if the vectors get to large as it is
impractical to deal with them any way other than programmatically.
-}

instance VTuple (Vec (d1:*.d2) a) (Quantity d1 a, Quantity d2 a) where
  toTuple v = (vElemAt zero v, vElemAt pos1 v)
  fromTuple (x,y) = vCons x $ vSing y

instance VTuple (Vec (d1:*:d2:*.d3) a) 
                (Quantity d1 a, Quantity d2 a, Quantity d3  a) where
  toTuple v = (vElemAt zero v, vElemAt pos1 v, vElemAt pos2 v)
  fromTuple (x,y,z) = vCons x $ vCons y $ vSing z


-- Querying
-- ========
-- | @vElem n vec@ returns the @n@:th element of @vec@. The index @n@
-- is zero-based. I could chose use an @HNat@ for indexing instead
-- of a @NumType@. It would simplify the type signatures but users of
-- dimensional are more likely to already have NumTypes in scope than
-- @HNat@s.
vElemAt :: (HLookupByHNat (AsHNat) n ds d) => n -> Vec ds a -> Quantity d a
vElemAt n (ListVec xs) = (xs!!toNum n) *~ siUnit


-- Homogenity
-- ==========
-- | This class guarantees that a vector is homogenuous w r t the
-- physical dimensions of its element.
class Homo ds d | ds -> d where
  -- | Converts a homogeneous vector to a list.
  toList :: Vec ds a -> [Quantity d a]
  toList (ListVec xs) = map (*~ siUnit) xs
instance Homo (HSing d) d
instance Homo (d:*:ds) d => Homo (d:*:(d:*:ds)) d


{-
Utility functions (do not export!)
==================================
Note that the type signatures permit coercion. The burden of verifying
consistency with type signature rests on user. Care must be taken
to specify expected/desired return type explicitly to be sure
expected results are obtained. These functions should not be exported
outside this module!
-}

-- | Map a function to the numeric representations of the elements
-- of a vector. IMPORTANT: v1 v2 must have the same length!
vMap :: (a -> b) -> Vec v1 a -> Vec v2 b
vMap f (ListVec xs) = ListVec (map f xs)

-- | Zip the numeric representation of the elements using the provided
-- function. IMPORTANT: v1 v2 v3 must have the same length!
vZipWith :: (a -> b -> c) -> Vec v1 a -> Vec v2 b -> Vec v3 c  -- Danger!
vZipWith f (ListVec v1) (ListVec v2) = ListVec (zipWith f v1 v2)

{-
TODO: At least ensure that the vectors are of the same length as
this ensures the mostimportant invariant. The disadvantage is that
it pollutes the type signatures of e.g. 'elemAdd'. Perhaps with a
different underlying implementation these utility functions can be
removed?

  vMap :: (HLength v1 n, HLength v2 n) => (a -> b) -> Vec v1 a -> Vec v2 b
  vZipWith :: (HLength v1 n, HLength v2 n, HLength v3 n)
           => (a -> b -> c) -> Vec v1 a -> Vec v2 b -> Vec v3 c
-}

-- Elementwise binary operators
-- ============================

-- | Elementwise addition of vectors. The vectors must have the
-- same size and element types.
elemAdd :: Num a => Vec ds a -> Vec ds a -> Vec ds a
elemAdd = vZipWith (P.+)

-- | Elementwise subraction of vectors. The vectors must have the
-- same size and element types.
elemSub :: Num a => Vec ds a -> Vec ds a -> Vec ds a
elemSub = vZipWith (P.-)


-- Elementwise multiplication of vectors
-- -------------------------------------

data MulD
instance Apply  MulD (d1, d2) (d1 * d2) where apply _ _ = undefined

-- | Multiplies each element i of the first argument vector by the
-- corresponding element of the second argument.
elemMul :: (HZipWith MulD ds1 ds2 ds3, Num a)
        => Vec ds1 a -> Vec ds2 a -> Vec ds3 a
elemMul = vZipWith (P.*)

{-

-- Elementwise division of vectors
-- -------------------------------

data DivD
instance Div d1 d2 d3 => Apply  DivD (d1, d2) d3 where apply _ _ = undefined

-- | Divides each element i of the first argument vector by the
-- corresponding element of the second argument.
elemDiv :: (HZipWith DivD ds1 ds2 ds3, Fractional a)
        => Vec ds1 a -> Vec ds2 a -> Vec ds3 a
elemDiv = vZipWith (P./)


-- Scaling of vectors
-- ==================
-- Should I change the order of arguments here so the vector always
-- comes first? Last?

instance Mul d1 d2 d3 => Apply (MulD, d1) d2  d3 where apply _ _ = undefined
instance Div d1 d2 d3 => Apply (DivD, d2) d1  d3 where apply _ _ = undefined

-- | Scale a vector by multiplication. Each element of the vector is
-- multiplied by the first argument.
scaleVec :: (HMap (MulD, d) ds1 ds2, Num a)
         => Quantity d a -> Vec ds1 a -> Vec ds2 a
scaleVec (Dimensional x) (ListVec xs) = ListVec (map (x P.*) xs)

-- | Scale a vector by a dimensionless quantity. This avoids the trivial
-- constraint @HMap (MulD, DOne) ds ds@ for this common case.
scaleVec1 :: Num a => Dimensionless a -> Vec ds a -> Vec ds a
scaleVec1 (Dimensional x) = vMap (x P.*)


-- Dot product
-- ===========
-- | This class allows calculating the dot product of two vectors assuming
-- they have suitable elements.
class DotProduct ds1 ds2 d | ds1 ds2 -> d where
  -- | Compute the dot product of two vectors.
  dotProduct :: Num a => Vec ds1 a -> Vec ds2 a -> Quantity d a
  dotProduct (ListVec xs1) (ListVec xs2) = Dimensional (O.sum_product xs1 xs2)
instance (HZipWith MulD ds1 ds2 ds3, Homo ds3 d) => DotProduct ds1 ds2 d


{-
Cross product
=============
Vector cross product is only applicable to vectors with three
elements (I believe). The constraints for the instance are brute-force.
It is slightly disconcerting that nothing prevents defining additional
nstances...
-}

class CrossProduct ds1 ds2 ds3 | ds1 ds2 -> ds3 where
  -- | Compute the cross product of two vectors.
  crossProduct :: Num a => Vec ds1 a -> Vec ds2 a -> Vec ds3 a
  crossProduct (ListVec [a, b, c]) (ListVec [d, e, f]) = ListVec
    [ b P.* f P.- e P.* c
    , c P.* d P.- f P.* a
    , a P.* e P.- d P.* b
    ]
instance (Mul b f g, Mul e c g, Mul c d h, Mul f a h, Mul a e i, Mul d b i)
  => CrossProduct (a:*:b:*.c)
                  (d:*:e:*.f)
                  (g:*:h:*.i)


-- Miscellaneous
-- =============

-- | Compute the sum of all elements in a homogenous vector.
vSum :: (Homo ds d, Num a) => Vec ds a -> Quantity d a
vSum (ListVec xs) = Dimensional (P.sum xs)

-- | Compute the vector norm.
--vNorm :: (Homo ds d, RealFloat a) => Vec ds a -> Quantity d' a
vNorm :: (DotProduct ds ds d, Root d Pos2 d', RealFloat a)
      => Vec ds a -> Quantity d' a
vNorm v = sqrt (v `dotProduct` v)

-- | Normalize a vector. The vector must be homogeneous.

vNormalize :: ( DotProduct ds ds d, Root d Pos2 d', Div DOne d' d''
              , HMap (MulD, d'') ds ds', RealFloat a ) => Vec ds a -> Vec ds' a
vNormalize v = (_1 / vNorm v) `scaleVec` v
-- -}
