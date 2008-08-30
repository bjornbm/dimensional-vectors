A representation of vectors where elements are quantities that may
have mixed physical dimensions. The goal is to check at compile
time that for any given operation the involved vectors have compatible
dimensions and that their elements have compatible physical dimensions.

In our initially implementation we use an inefficient internal
represenation of vectors based on plain lists. The intent is that
the internal representation can be transparently changed to something
more efficient (e.g.  GSLHaskell) once all the type trickery has
been worked out.

> {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

> module Vector
>   {- ( Vec
>   , vEmpty, vHead, vTail, vCons
>   , fromHList, toHList
>   ) -} where

> import Data.List (intercalate)
> import HList
> import MyHList
> import Numeric.Units.Dimensional (Dimensional (..), Quantity, Mul)
> import Numeric.Units.Dimensional.Prelude
> import qualified Prelude as P
> import qualified Orthogonals as O




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

> instance (ToHList (Vec ds a) l, HMapOut ShowElem l String) => Show (Vec ds a) where
>   show = (\s -> "< " ++ s ++ " >")
>        . intercalate ", "
>        . hMapOut ShowElem
>        . toHList


Construction
------------
Vectors can be constructed using 'vCons' and 'vSing' or 'fromHList'.

| Create a singleton vector.

> vSing :: Quantity d a -> Vec (HCons d HNil) a
> vSing (Dimensional x) = ListVec [x]

| Prepend an element to the vector.

> vCons :: Quantity d a -> Vec ds a -> Vec (HCons d ds) a
> vCons (Dimensional x) (ListVec xs) = ListVec (x:xs)

This class allows converting a HList to the equivalent vector.
TODO: The @HNil@ instance should be removed -- we do not want to allow creation
of empty vectors.

> class FromHList l x | l -> x where fromHList :: l -> x
> instance FromHList HNil (Vec HNil a) where fromHList _ = ListVec []
> instance FromHList l (Vec ds a) => FromHList (HCons (Quantity d a) l) (Vec (HCons d ds) a)
>   where fromHList (HCons x l) = vCons x (fromHList l)

This class allows converting a vector to an equivalent HList.

> class ToHList x l | x -> l where toHList :: x -> l
> instance ToHList (Vec HNil a) HNil where toHList _ = HNil
> instance ToHList (Vec ds a) l => ToHList (Vec (HCons d ds) a) (HCons (Quantity d a) l)
>   where toHList v = HCons (vHead v) (toHList $ vTail v)


Head and tail
-------------
| Return the first element of the vector.

> vHead :: Vec (HCons d ds) a -> Quantity d a
> vHead (ListVec xs) = Dimensional (head xs)

| Drop the first element of the vector.
TODO: The @HNil@ instance should be removed -- we do not want to allow creation
of empty vectors.

> vTail :: Vec (HCons d ds) a -> Vec ds a
> vTail (ListVec xs) = ListVec (tail xs)

| Unwrap a singular vector.

> --fromSing :: Vec (HCons d HNil) a -> Quantity d a
> --fromSing (ListVec [x]) = Dimensional x


Homogenity
==========
This class guarantees a vector is homogenuous w r t the physical
dimensions of its element.

> class Homo ds d | ds -> d where
>   toList :: Vec ds a -> [Quantity d a]
>   toList (ListVec xs) = map Dimensional xs
> --instance Homo HNil d
> --instance Homo ds d => Homo (HCons d ds) d
> instance Homo (HCons d HNil) d
> instance Homo (HCons d ds) d => Homo (HCons d (HCons d ds)) d


Dot product
===========

> data MulD = MulD
> instance Mul d1 d2 d3 => Apply  MulD (d1, d2) d3 where apply _ _ = undefined
> instance Mul d1 d2 d3 => Apply (MulD, d1) d2  d3 where apply _ _ = undefined

This class allows calculating the dot product of two vectors assuming
they have suitable elements.

> class DotProduct ds1 ds2 d | ds1 ds2 -> d where
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
>   crossProduct :: Num a => Vec ds1 a -> Vec ds2 a -> Vec ds3 a
>   crossProduct (ListVec [a, b, c]) (ListVec [d, e, f]) = ListVec
>     [ b P.* f P.- e P.* c
>     , c P.* d P.- f P.* a
>     , a P.* e P.- d P.* b
>     ]
> instance (Mul b f g, Mul e c g, Mul c d h, Mul f a h, Mul a e i, Mul d b i)
>   => CrossProduct (a :*: b :*: c :*: HNil)
>                   (d :*: e :*: f :*: HNil)
>                   (g :*: h :*: i :*: HNil)


Miscellaneous
=============
| The sum of all elements in a homogenous vector.

> vSum :: (Homo ds d, Num a) => Vec ds a -> Quantity d a
> vSum (ListVec xs) = Dimensional (P.sum xs)

Elementwise multiplication of vectors.

> -- class ElemMul ds1 ds2 ds3 | ds1 ds2 -> ds3 where
> --   elemMul :: Num a => Vec ds1 a -> Vec ds2 a -> Vec ds3 a
> --   elemMul (ListVec v1) (ListVec v2) = ListVec (zipWith (P.*) v1 v2)
> elemMul :: (HZipWith MulD ds1 ds2 ds3, Num a) => Vec ds1 a -> Vec ds2 a -> Vec ds3 a
> elemMul (ListVec v1) (ListVec v2) = ListVec (zipWith (P.*) v1 v2)

Elementwise addition of vectors.

> elemAdd :: Num a => Vec ds a -> Vec ds a -> Vec ds a
> elemAdd (ListVec v1) (ListVec v2) = ListVec (zipWith (P.+) v1 v2)

> elemSub :: Num a => Vec ds a -> Vec ds a -> Vec ds a
> elemSub (ListVec v1) (ListVec v2) = ListVec (zipWith (P.-) v1 v2)

Scale a vector (multiply with a scalar).

> {-
> class ScaleVec d ds1 ds2 | d ds1 -> ds2 where
>   scaleVec :: Num a => Quantity d a -> Vec ds1 a -> Vec ds2 a
>   scaleVec (Dimensional x) (ListVec xs) = ListVec (map (x P.*) xs)
> instance HMap (MulD, d) ds1 ds2 => ScaleVec d ds1 ds2
> -}
> scaleVec :: (HMap (MulD, d) ds1 ds2, Num a) => Quantity d a -> Vec ds1 a -> Vec ds2 a
> scaleVec (Dimensional x) (ListVec xs) = ListVec (map (x P.*) xs)



Test values
===========

> l1 = HCons (1.0 *~ meter) $ HCons (2.1 *~ second) $ HNil
> l2 = HCons (2.0 *~ hertz) $ HCons ((-0.1) *~ kilo gram) $ HNil
> l3 = HCons (133.0 *~ second) $ HCons (2.1 *~ meter) $ HNil
> l4 = (2.0 *~ hertz) .*. (22.0 *~ meter ^ neg1) .*. HNil

> v1 = fromHList l1
> v2 = fromHList l2
> v3 = fromHList l3
> v4 = fromHList l4
> v5 = vCons (3 *~ newton) v4

Testing crossProduct with homogeneous and heterogeneous vectors.

> vc1 = vCons (3 *~ meter) $ vCons (2 *~ meter) $ vSing (1 *~ meter)
> vc2 = vCons (1 *~ hertz) $ vCons (2 *~ hertz) $ vSing (3 *~ hertz)
> vc12 = crossProduct vc1 vc2

> vc3 = vCons (3.0 *~ meter)            $ vCons (2 *~ one)   $ vSing (1 *~ one)
> vc4 = vCons (1   *~ (meter / second)) $ vCons (2 *~ hertz) $ vSing (3 *~ hertz)
> vc34 = crossProduct vc3 vc4

