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

> {-# OPTIONS_GHC -fglasgow-exts #-}
> {-# LANGUAGE UndecidableInstances
>            , TypeOperators
>   #-}

> module Numeric.Units.Dimensional.LinearAlgebra.Vector
>   {- ( Vec
>   , vEmpty, vHead, vTail, vCons
>   , fromHList, toHList
>   ) -} where

> import Data.List (intercalate)
> import Data.HList
> import Numeric.Units.Dimensional.LinearAlgebra.HListExtras
> import Numeric.NumType (PosType, toNum, Pos2)
> import Numeric.Units.Dimensional.Prelude
> import Numeric.Units.Dimensional (Dimensional (..), Quantity, Mul)
> import qualified Orthogonals as O
> import qualified Prelude as P




Vector data type
================
A vector representation based on regular lists. The intent is that
the @ds@ type variable should be an HList of physical dimensions
corresponding to the elements of the list @[a]@. This will be an
abstract data type with constructor functions guaranteeing this
invariant.

> data Vec ds a = ListVec [a] deriving (Eq)

Showing
-------
We implement a custom @Show@ instance.

> data ShowElem = ShowElem
> instance Show a => Apply ShowElem a String where apply _ = show

> instance (VHList (Vec ds a) l, HMapOut ShowElem l String) => Show (Vec ds a)
>   where show = (\s -> "< " ++ s ++ " >")
>              . intercalate ", "
>              . hMapOut ShowElem
>              . toHList


Vector Construction and Deconstruction
======================================
Vectors can be primitively constructed using 'vCons' and 'vSing' and
deconstructed using 'vHead' and 'vTail'. We also provide type classes
for converting to/from HLists and tuples.

| Create a singleton vector.

> vSing :: Quantity d a -> Vec (HSing d) a
> vSing (Dimensional x) = ListVec [x]

| Prepend an element to the vector.

> vCons :: Quantity d a -> Vec ds a -> Vec (d:*:ds) a
> vCons (Dimensional x) (ListVec xs) = ListVec (x:xs)

| Return the first element of the vector.

> vHead :: Vec (d:*:ds) a -> Quantity d a
> vHead (ListVec xs) = Dimensional (head xs)

| Drop the first element of the vector.
of empty vectors.

> vTail :: Vec (d:*:ds) a -> Vec ds a  -- Can create empty vector.
> vTail (ListVec xs) = ListVec (tail xs)


Convert to/from HLists
----------------------
This class allows converting between vectors and the equivalent HLists.
of empty vectors.

> class VHList v l | v -> l, l -> v where
>     toHList   :: v -> l
>     fromHList :: l -> v

> instance VHList (Vec HNil a) HNil where  -- Can create empty vector.
>     toHList   _ = HNil
>     fromHList _ = ListVec []

> instance VHList (Vec ds a) l => VHList (Vec (d:*:ds) a) (Quantity d a:*:l)
>   where
>     toHList v = HCons (vHead v) (toHList $ vTail v)
>     fromHList (HCons x l) = vCons x (fromHList l)


Convert to/from Tuples
----------------------
| Convert to/from tuple representation. This is primarily to allow taking
advantage of the syntactic sugar tuples enjoy.

> class VTuple v t | v -> t, t -> v where
>   toTuple   :: v -> t
>   fromTuple :: t -> v

We can brute force the instances ut to a reasonable degree. Presumable
syntactic sugar loses its value if the vectors get to large as it is
impractical to deal with them any way other than programmatically.

> instance VTuple (Vec (d1:*.d2) a) (Quantity d1 a, Quantity d2 a) where
>   toTuple v = (vElemAt zero v, vElemAt pos1 v)
>   fromTuple (x,y) = vCons x $ vSing y

> instance VTuple (Vec (d1:*:d2:*.d3) a) 
>                 (Quantity d1 a, Quantity d2 a, Quantity d3  a) where
>   toTuple v = (vElemAt zero v, vElemAt pos1 v, vElemAt pos2 v)
>   fromTuple (x,y,z) = vCons x $ vCons y $ vSing z


Querying
========
| @vElem n vec@ returns the @n@:th element of @vec@. The index @n@ is zero-based. I could chose use an HNat for indexing instead of a NumType. It would simplify the type signatures but users are more likely to already have NumTypes in scope than HNats.

> vElemAt :: (HNatNumType n' n, HLookupByHNat n' ds d)
>         => n -> Vec ds a -> Quantity d a
> vElemAt n (ListVec xs) = Dimensional (xs!!toNum n)


Homogenity
==========
| This class guarantees that a vector is homogenuous w r t the physical
dimensions of its element.

> class Homo ds d | ds -> d where
>   -- | Converts a homogeneous vector to a list.
>   toList :: Vec ds a -> [Quantity d a]
>   toList (ListVec xs) = map Dimensional xs
> instance Homo (HSing d) d
> instance Homo (d:*:ds) d => Homo (d:*:(d:*:ds)) d

The above instances ensure that the vector has at least one element. An optional implementation would be to use the below instances without this guarantee.

  instance Homo HNil d
  instance Homo ds d => Homo (HCons d ds) d


Utility functions (do not export!)
==================================
Note that the type signatures permit coercion. The burden of verifying consistency with type signature rests on user. Care must be taken to specify expected/desired return type explicitly to be sure expected results are obtained. These functions should not be exported outside this module!

Map a function to the numeric representations of the elements of a vector.

> vMap :: (a -> b) -> Vec ds a -> Vec ds' b
> vMap f (ListVec xs) = ListVec (map f xs)

Note the lack of type signature permits dangerous coersion. Burden of verifying signature rests on user. Therefore this function should not be exported outside this module.
TODO: At least ensure that the vectors are of the same length as this ensures the most important invariant.

> --vZipWith :: (HLength ds n, HLength ds' n, HLength ds'' n) => (a -> b -> c) -> Vec ds a -> Vec ds' b -> Vec ds'' c
> vZipWith :: (a -> b -> c) -> Vec ds a -> Vec ds' b -> Vec ds'' c  -- Danger!
> vZipWith f (ListVec v1) (ListVec v2) = ListVec (zipWith f v1 v2)


Elementwise binary operators
============================

| Elementwise addition of vectors. The vectors must have the same size and element types.

> elemAdd :: Num a => Vec ds a -> Vec ds a -> Vec ds a
> elemAdd = vZipWith (P.+)

| Elementwise subraction of vectors. The vectors must have the same size and element types.

> elemSub :: Num a => Vec ds a -> Vec ds a -> Vec ds a
> elemSub = vZipWith (P.-)


Elementwise multiplication of vectors
-------------------------------------

> data MulD
> instance Mul d1 d2 d3 => Apply  MulD (d1, d2) d3 where apply _ _ = undefined

| Multiplies each element i of the first argument vector by the corresponding element of the second argument.

> elemMul :: (HZipWith MulD ds1 ds2 ds3, Num a)
>         => Vec ds1 a -> Vec ds2 a -> Vec ds3 a
> elemMul = vZipWith (P.*)

Elementwise division of vectors
-------------------------------

> data DivD
> instance Div d1 d2 d3 => Apply  DivD (d1, d2) d3 where apply _ _ = undefined

| Divides each element i of the first argument vector by the corresponding element of the second argument.

> elemDiv :: (HZipWith DivD ds1 ds2 ds3, Fractional a)
>         => Vec ds1 a -> Vec ds2 a -> Vec ds3 a
> elemDiv = vZipWith (P./)


Scaling of vectors
==================
Should I change the order of arguments here so the vector always
comes first? Last?

> instance Mul d1 d2 d3 => Apply (MulD, d1) d2  d3 where apply _ _ = undefined
> instance Div d1 d2 d3 => Apply (DivD, d2) d1  d3 where apply _ _ = undefined

| Scale a vector by multiplication. Each element of the vector is
multiplied by the first argument.

> scaleVec :: (HMap (MulD, d) ds1 ds2, Num a)
>          => Quantity d a -> Vec ds1 a -> Vec ds2 a
> scaleVec (Dimensional x) (ListVec xs) = ListVec (map (x P.*) xs)

| Scale a vector by a dimensionless quantity. This avoids the trivial
constraint @HMap (MulD, DOne) ds ds@ for this common case.

> scaleVec1 :: Num a => Dimensionless a -> Vec ds a -> Vec ds a
> scaleVec1 (Dimensional x) = vMap (x P.*)


Dot product
===========
| This class allows calculating the dot product of two vectors assuming
they have suitable elements.

> class DotProduct ds1 ds2 d | ds1 ds2 -> d where
>   -- | Compute the dot product of two vectors.
>   dotProduct :: Num a => Vec ds1 a -> Vec ds2 a -> Quantity d a
>   dotProduct (ListVec xs1) (ListVec xs2) = Dimensional (O.sum_product xs1 xs2)
> instance (HZipWith MulD ds1 ds2 ds3, Homo ds3 d) => DotProduct ds1 ds2 d


Cross product
=============
Vector cross product is only applicable to vectors with three
elements (I believe). The constraints for the instance are brute-force.
It is slightly disconcerting that nothing prevents defining additional
instances...

> class CrossProduct ds1 ds2 ds3 | ds1 ds2 -> ds3 where
>   -- | Compute the cross product of two vectors.
>   crossProduct :: Num a => Vec ds1 a -> Vec ds2 a -> Vec ds3 a
>   crossProduct (ListVec [a, b, c]) (ListVec [d, e, f]) = ListVec
>     [ b P.* f P.- e P.* c
>     , c P.* d P.- f P.* a
>     , a P.* e P.- d P.* b
>     ]
> instance (Mul b f g, Mul e c g, Mul c d h, Mul f a h, Mul a e i, Mul d b i)
>   => CrossProduct (a:*:b:*.c)
>                   (d:*:e:*.f)
>                   (g:*:h:*.i)


Miscellaneous
=============
| Compute the sum of all elements in a homogenous vector.

> vSum :: (Homo ds d, Num a) => Vec ds a -> Quantity d a
> vSum (ListVec xs) = Dimensional (P.sum xs)

| Compute the vector norm.

> --vNorm :: (Homo ds d, RealFloat a) => Vec ds a -> Quantity d' a
> vNorm :: (DotProduct ds ds d, Root d Pos2 d', RealFloat a)
>       => Vec ds a -> Quantity d' a
> vNorm v = sqrt (v `dotProduct` v)

| Normalize a vector. The vector must be homogeneous.

> vNormalize :: (DotProduct ds ds d, Root d Pos2 d', Div DOne d' d'', HMap (MulD, d'') ds ds', RealFloat a) => Vec ds a -> Vec ds' a
> vNormalize v = (_1 / vNorm v) `scaleVec` v
