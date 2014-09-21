{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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

module MyHList where

import qualified Prelude as P
import Data.List (intercalate)
import Data.Proxy
import GHC.TypeLits hiding (type (*))
import Numeric.Units.Dimensional.DK.Prelude
import Apply
import Nats

-- $setup
-- >>> let x = 2 *~ meter :: Length Double
-- >>> let y = 3 *~ kilo gram :: Mass Double
-- >>> let z = _1
-- >>> let v = x <: y <:. z
-- >>> let v' = 2 *~ meter <: 3 *~ kilo gram <:. _1
-- >>> let vh1 = x <: x <:. x
-- >>> let v''' = y <: x <:. x*y
-- >>> let vc3 = 3.0 *~ meter <: (2 *~ one) <:. (1 *~ one)
-- >>> let vc4 = 1 *~ (meter / second) <: 2 *~ hertz <:. 3 *~ hertz

infixr 5  <:, <:.

-- | The vector type.
  --
  -- Currently represented as a vanilla list. Will change to more
  -- powerful representation once the API is stable.
newtype Vec (ds::[Dimension]) a = ListVec [a] deriving (Eq)

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
  --
  -- >>> repMap (P.* 2) v == vMap (UnaryR Mul (_2::Dimensionless Double)) v
  -- True
repMap :: (a -> b) -> Vec v1 a -> Vec v2 b
repMap f (ListVec xs) = ListVec (map f xs)

-- | Zip the numeric representation of the elements using the provided
-- function. IMPORTANT: v1 v2 v3 must have the same length!
  --
  -- >>> repZipWith (P.*) v v == vZipWith Mul v v
  -- True
  --
repZipWith :: -- (VLength v1 ~ VLength v2, VLength v1 ~ VLength v3) =>
              (a -> b -> c) -> Vec v1 a -> Vec v2 b -> Vec v3 c  -- Danger!
repZipWith f (ListVec v1) (ListVec v2) = ListVec (zipWith f v1 v2)
{-
TODO: At least ensure that the vectors are of the same length as
this ensures the most important invariant. The disadvantage is that
it pollutes the type signatures of e.g. 'elemAdd'. Perhaps with a
different underlying implementation these utility functions can be
removed?
-}


-- Vector construction
-- ===================
-- Exported constructor functions.

-- | Construct a vector with a single element.
  --
  -- >>> vSing x
  -- < 2.0 m >
vSing :: Fractional a => Quantity d a -> Vec '[d] a
vSing x = ListVec [x /~ siUnit]

-- | Create a vector with two elements.
  --
  -- >>> x <:. y
  -- < 2.0 m, 3.0 kg >
  -- >>> x <:. y == x <: vSing y
  -- True
  -- >>> x <:. y == vSing x `vAppend` vSing y
  -- True
  -- >>> x <:. y == vSing x `vSnoc` y
  -- True
(<:.) :: Fractional a => Quantity d1 a -> Quantity d2 a -> Vec '[d1, d2] a
x <:. y = ListVec [x /~ siUnit, y /~ siUnit]


-- List construction style (cons)
-- ------------------------------

-- | Prepend an element to a vector.
  --
  -- >>> vCons x (vSing y)
  -- < 2.0 m, 3.0 kg >
(<:), vCons  :: Fractional a => Quantity d a -> Vec ds a -> Vec (d ': ds) a
x <: ListVec xs = ListVec (x /~ siUnit : xs)
vCons = (<:)


-- Append and snoc
-- ---------------

type family VAppend (ds1::[Dimension]) (ds2::[Dimension]) :: [Dimension]
  where
    VAppend '[d]       ds  = d ': ds
    VAppend (d ': ds1) ds2 = d ': VAppend ds1 ds2

-- | Append the second vector to the first.
  --
  -- >>> vAppend v v
  -- < 2.0 m, 3.0 kg, 1.0, 2.0 m, 3.0 kg, 1.0 >
vAppend :: Vec ds1 a -> Vec ds2 a -> Vec (VAppend ds1 ds2) a
vAppend (ListVec xs) (ListVec ys) = ListVec (xs ++ ys)


type VSnoc ds d = VAppend ds '[d]

-- | Append a quantity to the end of a vector.
  --
  -- >>> vSnoc (vSing x) y
  -- < 2.0 m, 3.0 kg >
  -- >>> vSnoc v x
  -- < 2.0 m, 3.0 kg, 1.0, 2.0 m >
  -- >>> vSnoc v x == vAppend v (vSing x)
  -- True
vSnoc :: Fractional a => Vec ds a -> Quantity d a -> Vec (VSnoc ds d) a
vSnoc (ListVec xs) x = ListVec (xs ++ [x /~ siUnit])

-- | Principled implementation of 'vSnoc'.
  --
  -- >>> vSnoc v x == vSnoc' v x
  -- True
vSnoc' :: Fractional a => Vec ds a -> Quantity d a -> Vec (VSnoc ds d) a
vSnoc' v x = vAppend v (vSing x)


-- Deconstruction
-- ==============

-- List style deconstruction (head, tail)
-- --------------------------------------

-- | Return the first element of the vector.
  --
  -- >>> vHead v
  -- 2.0 m
vHead :: Num a => Vec ds a -> Quantity (VHead ds) a
vHead (ListVec (x:xs)) = x *~ siUnit

-- | Principled implementation of 'vHead'.
  --
  -- >>> vHead v == vHead' v
  -- True
vHead' :: Num a => Vec (d ': ds) a -> Quantity d a
vHead' = vElemAt nat0

type family VHead (ds::[Dimension]) :: Dimension
  where VHead (d ': ds) = d

-- | Drop the first element of the vector.
  --
  -- >>> vTail v
  -- < 3.0 kg, 1.0 >
vTail :: Vec ds a -> Vec (VTail ds) a
--vTail :: Vec (d1 ': d2 ': ds) a -> Vec (d2 ': ds) a
vTail (ListVec xs) = ListVec (tail xs)

type family VTail (ds::[Dimension]) :: [Dimension]
  where VTail (d1 ': d2 ': ds) = d2 ': ds


-- Dlist style (last, init)
-- ------------------------

type family VInit ds :: [Dimension] where
  VInit '[d1, d2] = '[d1]
  VInit (d ': ds) = d ': VInit ds

-- | Drop the last element of the vector.
  --
  -- >>> vInit v == x <:. y
  -- True
  -- >>> vInit (vSnoc v x) == v
  -- True
vInit :: Vec ds a -> Vec (VInit ds) a
vInit (ListVec xs) = ListVec (init xs)

type family VLast ds :: Dimension where
  VLast '[d] = d
  VLast (d ': ds) = VLast ds

-- | Return the last element of the vector.
  --
  -- >>> vLast v == z
  -- True
  -- >>> vLast (vSnoc v x) == x
  -- True
vLast :: Num a => Vec ds a -> Quantity (VLast ds) a
vLast (ListVec xs) = last xs *~ siUnit


-- Element lookup
-- --------------
--deriving instance Show a => Show (Vec ds a)
type family VElemAt (n::Nat) (ds::[Dimension]) :: Dimension
  where
    VElemAt 0 (d ': ds) = d
    VElemAt n (d ': ds) = VElemAt (n - 1) ds

-- | Look up the element at the given (zero-based) index.
  -- >>> vElemAt nat0 v == x
  -- True
  -- >>> vElemAt nat1 v == y
  -- True
  -- >>> vElemAt nat2 v == z
  -- True
vElemAt :: (KnownNat n, Num a)
        => Proxy (n::Nat) -> Vec ds a -> Quantity (VElemAt n ds) a
vElemAt n (ListVec xs) = (xs !! fromInteger (natVal n)) *~ siUnit



-- Unary (single vector) operations
-- ================================

-- Length
-- ------

type family VLength (ds::[Dimension]) :: Nat
  where
    VLength '[d] = 1
    VLength (d ': ds) = VLength ds + 1

-- | Return the length of a vector.
  -- >>> vLength v == nat3
  -- True
vLength :: Vec ds a -> Proxy (VLength ds)
vLength _ = Proxy


-- Forth style rot
-- ---------------

-- | Rotate a vector so that @<x,y,z> -> <y,z,x>@.
  --
  -- >>> rot v
  -- < 3.0 kg, 1.0, 2.0 m >
  -- >>> rot v == (vTail v `vSnoc` vHead v)
  -- True
  -- >>> (rot $ rot $ rot v) == v
  -- True
rot :: Fractional a => Vec ds a -> Vec (Rot ds) a
rot (ListVec (x:xs)) = ListVec (xs ++ [x])

type Rot ds = VSnoc (VTail ds) (VHead ds)

-- | Principled implementation of 'rot'.
  --
  -- >>> rot v == rot' v
  -- True
rot' :: Fractional a => Vec (d ': d2 ': ds) a -> Vec (VSnoc (d2 ': ds) d) a
rot' v = vTail v `vSnoc` vHead v



-- Scaling
-- -------

type ScaleVec d ds a = VMap (UnaryR Mul d a) ds

-- | Scale a vector by multiplication. Each element of the vector is
  -- multiplied by the first argument.
  --
  -- >>> scaleVec (2*~gram) (vSing $ 3 *~ meter)
  -- < 6.0e-3 m kg >
  -- >>> scaleVec (2*~gram) (_4 <:. 3 *~ meter)
  -- < 8.0e-3 kg, 6.0e-3 m kg >
  --
  -- TODO convert to prop.
  --
  -- >>> scaleVec x v == vMap (UnaryR Mul x) v
  -- True
scaleVec :: Fractional a
         => Quantity d a -> Vec ds a -> Vec (ScaleVec d ds a) a
scaleVec x v = repMap (P.* (x /~ siUnit)) v

-- | Principled implementation of 'scaleVec'.
  --
  -- >>> scaleVec x v == scaleVec' x v
  -- True
scaleVec' :: (VMapC (UnaryR Mul d a) ds a, Fractional a)
          => Quantity d a -> Vec ds a -> Vec (ScaleVec d ds a) a
scaleVec' x = vMap (UnaryR Mul x)  -- Rigorious implementation.

-- | Scale a vector by a dimensionless quantity. This avoids the trivial
  -- constraint @HMap (MulD, DOne) ds ds@ for this common case.
  --
  -- >>> scaleVec1 _2 (_4 <:. 3 *~ meter)
  -- < 8.0, 6.0 m >
  -- >>> scaleVec y v == vMap (UnaryR Mul y) v
  -- True
scaleVec1 :: Fractional a => Dimensionless a -> Vec ds a -> Vec ds a
scaleVec1 x v = repMap (P.* (x /~ one)) v


-- Elementwise binary operators
-- ============================

-- | Elementwise addition of vectors. The vectors must have the
-- same size and element types.
  --
  -- >>> elemAdd v v == scaleVec _2 v
  -- True
elemAdd :: Num a => Vec ds a -> Vec ds a -> Vec ds a
elemAdd = repZipWith (P.+)

-- | Principled implementation of 'elemAdd'.
  --
  -- >>> elemAdd v v == elemAdd' v v
  -- True
elemAdd' :: (Num a, VZipWithC Add ds ds a, VZipWith Add ds ds ~ ds)
         => Vec ds a -> Vec ds a -> Vec ds a
elemAdd' v1 v2 = vZipWith Add v1 v2


-- | Elementwise subraction of vectors. The vectors must have the
-- same size and element types.
  --
  -- >>> elemSub v v == scaleVec _0 v
  -- True
elemSub :: Num a => Vec ds a -> Vec ds a -> Vec ds a
elemSub = repZipWith (P.-)

-- | Principled implementation of 'elemSub'.
  --
  -- >>> elemSub v v == elemSub' v v
  -- True
elemSub' :: (Num a, VZipWithC Sub ds ds a, VZipWith Sub ds ds ~ ds)
         => Vec ds a -> Vec ds a -> Vec ds a
elemSub' v1 v2 = vZipWith Sub v1 v2


-- | Elementwise multiplication of vectors.
  -- Multiplies each element i of the first argument vector by the
  -- corresponding element of the second argument.
  --
  -- >>> elemMul v v == vZipWith Mul v v
  -- True
elemMul :: Num a => Vec ds a -> Vec es a -> Vec (VZipWith Mul ds es) a
elemMul = repZipWith (P.*)

-- | Principled implementation of elemMul'.
  --
  -- >>> elemMul v v' == elemMul' v v'
  -- True
elemMul' :: (VZipWithC Mul ds es a, Num a)
         => Vec ds a -> Vec es a -> Vec (VZipWith Mul ds es) a
elemMul' = vZipWith Mul


-- | Elementwise division of vectors 
  -- Divides each element i of the first argument vector by the
  -- corresponding element of the second argument.
  --
  -- >>> elemDiv v v == vZipWith Div v v
  -- True
elemDiv :: Fractional a => Vec ds a -> Vec es a -> Vec (VZipWith Div ds es) a
elemDiv = repZipWith (P./)

-- | Principled implementation of elemDiv'.
  --
  -- >>> elemDiv v v' == elemDiv' v v'
  -- True
elemDiv' :: (VZipWithC Div ds es a, Num a)
         => Vec ds a -> Vec es a -> Vec (VZipWith Div ds es) a
elemDiv' = vZipWith Div



-- Higher order functions
-- ======================

-- |
-- >>> mapOut Show (vSing $ 32.3 *~ meter) :: [String]
-- ["32.3 m"]
--
-- >>> mapOut Show (2 *~ gram <: _3 <:. 32.3 *~ meter) :: [String]
-- ["2.0e-3 kg","3.0","32.3 m"]
--
-- >>> show (2 *~ gram <: _3 <:. 32.3 *~ meter)
-- "< 2.0e-3 kg, 3.0, 32.3 m >"
instance (MapOutC Show' ds a, MapOut Show' ds a ~ String) => Show (Vec ds a)
  where show = (\s -> "< " ++ s ++ " >")
             . intercalate ", "
             . mapOut Show


-- | Map out a vector to a list.
class MapOutC f (ds::[Dimension]) a where
  type MapOut f ds a
  mapOut :: f -> Vec ds a -> [MapOut f ds a]

instance (ApplyC f d a, Num a) => MapOutC f '[d] a where
  type MapOut f '[d] a = Apply f d a
  mapOut f v = [apply f $ vHead v]

instance ( ApplyC f d1 a, MapOutC f (d2 ': ds) a, Num a
         , Apply f d1 a ~ MapOut f (d2 ': ds) a)
        => MapOutC f (d1 ': d2 ': ds) a
  where
    type MapOut f (d1 ': d2 ': ds) a = MapOut f (d2 ': ds) a -- :Apply f d a
    mapOut f v = apply f (vHead v) : mapOut f (vTail v)

-- --------------------------------------------------------------
-- --------------------------------------------------------------
-- |
  --
class VMapC f ds1 a where
  type VMap f ds1 :: [Dimension]
  vMap :: f -> Vec ds1 a -> Vec (VMap f ds1) a

instance (UnaryC f d a, Fractional a) => VMapC f '[d] a where
  type VMap f '[d] = '[Unary f d]
  vMap f = vSing . unary f . vHead

instance (UnaryC f d1 a, VMapC f (d2 ': ds) a, Fractional a) => VMapC f (d1 ': d2 ': ds) a
  where
    type VMap f (d1 ': d2 ': ds) = Unary f d1 ': VMap f (d2 ': ds)
    vMap f v = unary f (vHead v) <: vMap f (vTail v)



class VZipWithC f ds1 ds2 a where
  type VZipWith f ds1 ds2 :: [Dimension]
  -- |
    -- >>> vZipWith Mul (vSing x) (vSing z) == vSing (x * z)
    -- True
    -- >>> vZipWith Mul v v == (x * x) <: (y * y) <:. (z * z)
    -- True
  vZipWith :: f -> Vec ds1 a -> Vec ds2 a -> Vec (VZipWith f ds1 ds2) a


instance (BinaryC f d1 d2 a, Fractional a) => VZipWithC f '[d1] '[d2] a where
  type VZipWith f '[d1] '[d2] = '[Binary f d1 d2]
  vZipWith f v1 v2 = vSing $ binary f (vHead v1) (vHead v2)

instance (VZipWithC f (d2 ': ds) (e2 ': es) a, BinaryC f d1 e1 a, Fractional a)
      => VZipWithC f (d1 ': d2 ': ds) (e1 ': e2 ': es) a
  where
    type VZipWith f (d1 ': d2  ': ds) (e1 ': e2 ': es) = Binary f d1 e1 ': VZipWith f (d2 ': ds) (e2 ': es)
    vZipWith f v1 v2 = binary f (vHead v1) (vHead v2) <: vZipWith f (vTail v1) (vTail v2)



-- Homogeneous vectors
-- ===================

class HomoC (ds::[Dimension]) where type Homo ds :: Dimension
instance HomoC '[d] where type Homo '[d] = d
instance (Homo ds ~ d) => HomoC (d ': ds) where type Homo (d ': ds) = d


-- | Convert a homogeneous vector to a list.
  --
  -- >>> toList vh1
  -- [2.0 m,2.0 m,2.0 m]
  -- >>> toList vh1 == mapOut Id vh1
  -- True
toList :: (HomoC ds, Num a) => Vec ds a -> [Quantity (Homo ds) a]
toList (ListVec xs) = xs *~~ siUnit

-- | Principled implementation of 'toList'.
  --
  -- >>> toList vh1 == toList' vh1
  -- True
toList' :: (MapOutC Id ds a) => Vec ds a -> [MapOut Id ds a]
toList' = mapOut Id


-- | Compute the sum of all elements in a homogeneous vector.
  --
  -- >>> vSum vh1 == sum (toList vh1)
  -- True
  -- >>> vSum vh1
  -- 6.0 m
vSum :: Num a => Vec ds a -> Quantity (Homo ds) a
vSum (ListVec xs) = P.sum xs *~ siUnit

-- | Principled implementation of 'sum'.
  --
  -- >>> vSum vh1 == vSum' vh1
  -- True
vSum' :: (HomoC ds, Num a) => Vec ds a -> Quantity (Homo ds) a
vSum' = sum . toList


-- Dot product
-- ===========

-- Approach using constraint synonyms.
type DotProductC ds1 ds2 = HomoC (VZipWith Mul ds1 ds2)
type DotProduct  ds1 ds2 = Homo  (VZipWith Mul ds1 ds2)

-- | Compute the dot product of two vectors.
  --
  -- >>> dotProduct v v''' == vSum (elemMul v v''')
  -- True
  -- >>> dotProduct v v'''
  -- 18.0 m kg
dotProduct :: Num a =>
  Vec ds1 a -> Vec ds2 a -> Quantity (DotProduct ds1 ds2) a
dotProduct (ListVec xs) (ListVec ys) = P.sum (zipWith (P.*) xs ys) *~ siUnit

-- | Principled implementation of 'dotProduct'.
  --
  -- >>> dotProduct v v''' == dotProduct' v v'''
  -- True
dotProduct' :: (DotProductC ds1 ds2, Num a) =>
  Vec ds1 a -> Vec ds2 a -> Quantity (DotProduct ds1 ds2) a
dotProduct' v1 v2 = vSum (elemMul v1 v2)


-- Cross Product
-- =============
  {-
class CrossProductC (ds1::[Dimension]) (ds2::[Dimension]) where
  type CrossProduct ds1 ds2 :: [Dimension]
  -- | Compute the cross product of two vectors with three elements.
  crossProduct :: Num a => Vec ds1 a -> Vec ds2 a -> Vec (CrossProduct ds1 ds2) a

instance ((b*f) ~ (e*c), (c*d) ~ (a*f), (a*e) ~ (d*b))
  => CrossProductC [a,b,c] [d,e,f] where
  type CrossProduct [a,b,c] [d,e,f] = [b * f, c * d, a * e]
  crossProduct (ListVec [a,b,c]) (ListVec [d,e,f]) = ListVec
    [ b P.* f P.- e P.* c
    , c P.* d P.- f P.* a
    , a P.* e P.- d P.* b
    ]
    -- -}

--type family CrossProductC ds1 ds2 where
type family CrossProduct ds1 ds2 where
  CrossProduct '[b,c,d] '[e,f,g] = '[c*g, d*e, b*f]
type CrossProductC ds1 ds2 = CrossProduct ds1 ds2 ~ CrossProduct ds2 ds1

-- | Compute the cross product of two vectors with three elements.
  --
  -- >>> crossProduct vc3 vc4 == vMap Neg (crossProduct vc4 vc3)
  -- True
  --
--crossProduct :: (Fractional a, (c*g) ~ (f*d), (d*e) ~ (g*b), (b*f) ~ (e*c)) --              => Vec '[b,c,d] a -> Vec '[e,f,g] a -> Vec '[c*g, d*e, b*f] a
crossProduct :: (CrossProductC ds1 ds2, Fractional a)
              => Vec ds1 a -> Vec ds2 a -> Vec (CrossProduct ds1 ds2) a
crossProduct (ListVec [a,b,c]) (ListVec [d,e,f]) = ListVec
    [ b P.* f P.- e P.* c
    , c P.* d P.- f P.* a
    , a P.* e P.- d P.* b
    ]


-- |
  -- >>> crossProduct vc3 vc4 == crossProduct''' vc3 vc4
  -- True
type family CrossProductC' ds1 ds2 where
  CrossProductC' '[b,c,d] '[e,f,g] = ((c*g) ~ (f*d), (d*e) ~ (g*b), (b*f) ~ (e*c))

crossProduct''' :: (Fractional a, (c*g) ~ (f*d), (d*e) ~ (g*b), (b*f) ~ (e*c))
              => Vec '[b,c,d] a -> Vec '[e,f,g] a -> Vec '[c*g, d*e, b*f] a
crossProduct''' v1 v2 = rot (cp v1 v2 `elemSub` cp v2 v1)
  where cp v1 v2 = v1 `elemMul` rot v2



-- | Principled implementation of 'crossProduct'.
  --
  -- >>> crossProduct vh1 vh1 == crossProduct' vh1 vh1
  -- True
  -- >>> crossProduct vc3 vc4 == crossProduct' vc3 vc4
  -- True
crossProduct' :: (Fractional a, (c*g) ~ (f*d), (d*e) ~ (g*b), (b*f) ~ (e*c))
              => Vec '[b,c,d] a -> Vec '[e,f,g] a -> Vec '[c*g, d*e, b*f] a
crossProduct' v1 v2 =  (c * g - f * d)
                   <:  (d * e - g * b)
                   <:. (b * f - e * c)
  where
    b = vElemAt nat0 v1
    c = vElemAt nat1 v1
    d = vElemAt nat2 v1
    e = vElemAt nat0 v2
    f = vElemAt nat1 v2
    g = vElemAt nat2 v2
