{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MyHList where

import qualified Prelude as P
import Data.List (intercalate)
import Data.Proxy
import GHC.TypeLits hiding (type (*))
import Numeric.Units.Dimensional.DK.Prelude

-- $setup
-- >>> :set -XDataKinds
-- >>> let nat0 = Proxy :: Proxy 0
-- >>> let nat1 = Proxy :: Proxy 1
-- >>> let nat2 = Proxy :: Proxy 2
-- >>> let nat3 = Proxy :: Proxy 3
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


newtype Vec (ds::[Dimension]) a = ListVec [a] deriving (Eq)

-- Exported constructor functions.
-- |
  -- >>> vSing x
  -- < 2.0 m >
vSing :: Fractional a => Quantity d a -> Vec '[d] a
vSing x = ListVec [x /~ siUnit]

-- |
  -- >>> vCons x (vSing y)
  -- < 2.0 m, 3.0 kg >
(<:), vCons  :: Fractional a => Quantity d a -> Vec ds a -> Vec (d ': ds) a
x <: ListVec xs = ListVec (x /~ siUnit : xs)
vCons = (<:)

-- |
  -- >>> x <:. y
  -- < 2.0 m, 3.0 kg >
(<:.) :: Fractional a => Quantity d1 a -> Quantity d2 a -> Vec '[d1, d2] a
x <:. y = x <: vSing y -- = ListVec [x /~ siUnit, y /~ siUnit]

-- | Return the first element of the vector.
  -- >>> vHead v
  -- 2.0 m
vHead :: Num a => Vec (d ': ds) a -> Quantity d a
vHead = vElemAt nat0

-- | Drop the first element of the vector.
  -- >>> vTail v
  -- < 3.0 kg, 1.0 >
vTail :: Vec (d1 ': d2 ': ds) a -> Vec (d2 ': ds) a
vTail (ListVec xs) = ListVec (tail xs)


{-
-- Convert to/from Tuples
-- ----------------------
-- | Convert to/from tuple representation. This is primarily to allow taking
-- advantage of the syntactic sugar tuples enjoy.
class VTupleC v t | v -> t, t -> v where
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
-}

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

-- Elementwise binary operators
-- ============================

-- | Elementwise addition of vectors. The vectors must have the
-- same size and element types.
  --
  -- >>> elemAdd v v == scaleVec _2 v
  -- True
elemAdd :: Num a => Vec ds a -> Vec ds a -> Vec ds a
elemAdd = repZipWith (P.+)

-- | Elementwise subraction of vectors. The vectors must have the
-- same size and element types.
  --
  -- >>> elemSub v v == scaleVec _0 v
  -- True
elemSub :: Num a => Vec ds a -> Vec ds a -> Vec ds a
elemSub = repZipWith (P.-)


-- Length
-- ======

type family VLength (ds::[Dimension]) :: Nat
  where
    VLength '[d] = 1
    VLength (d ': ds) = VLength ds + 1

-- |
  -- >>> vLength v == nat3
  -- True
vLength :: Vec ds a -> Proxy (VLength ds)
vLength _ = Proxy

-- Lookup
-- ======
--deriving instance Show a => Show (Vec ds a)
type family VElemAt (n::Nat) (ds::[Dimension]) :: Dimension
  where
    VElemAt 0 (d ': ds) = d
    VElemAt n (d ': ds) = VElemAt (n - 1) ds

-- |
  -- >>> vElemAt nat0 v == x
  -- True
  -- >>> vElemAt nat1 v == y
  -- True
  -- >>> vElemAt nat2 v == z
  -- True
vElemAt :: (KnownNat n, Num a)
        => Proxy (n::Nat) -> Vec ds a -> Quantity (VElemAt n ds) a
vElemAt n (ListVec xs) = (xs !! fromInteger (natVal n)) *~ siUnit


-- Apply with type.
class ApplyC f d a where
  type Apply f d a
  apply :: f -> Quantity d a -> Apply f d a

instance ApplyC Id d a where
  type Apply Id d a = Quantity d a
  apply _ = id


{-
-- Apply with class.
class ApplyC f d a b where applyC :: f -> Quantity d a -> b

instance ( UnaryC f d a, d2 ~ Unary f d
  ) => ApplyC f d a (Quantity d2 a)
  where
    applyC = unary
-}

-- |
-- >>> apply Show (4.2 *~ kilo meter) :: String
-- "4200.0 m"
--
-- >>> apply Show (42 *~ gram)
-- "4.2e-2 kg"
--
-- >>> show (2 *~ gram <: _3 <:. 32.3 *~ meter)
-- "< 2.0e-3 kg, 3.0, 32.3 m >"
--
data ShowQ = Show

instance Show (Quantity d a) => ApplyC ShowQ d a where
  type Apply ShowQ d a = String
  apply _ = show

--instance Show (Quantity d a) => ApplyC ShowQ d a String where applyC _ = show


-- |
-- >>> mapOut Show (vSing $ 32.3 *~ meter) :: [String]
-- ["32.3 m"]
--
-- >>> mapOut Show (2 *~ gram <: _3 <:. 32.3 *~ meter) :: [String]
-- ["2.0e-3 kg","3.0","32.3 m"]
--
instance (MapOutC ShowQ ds a, MapOut ShowQ ds a ~ String) => Show (Vec ds a)
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
-- --------------------------------------------------------------

class UnaryC f d a where
  type Unary f d :: Dimension
  unary :: f -> Quantity d a -> Quantity (Unary f d) a

-- |
class BinaryC f d1 d2 a where
  type Binary f d1 d2 :: Dimension
  binary :: f -> Quantity d1 a -> Quantity d2 a -> Quantity (Binary f d1 d2) a

-- | Type for making a binary operation unary, with the left argument
  -- pre-supplied.
  --
  -- >>> unary (UnaryL x Div) y == binary Div x y
  -- True
data UnaryL d a f = UnaryL (Quantity d a) f

-- | Type for making a binary operation unary, with the right argument
  -- pre-supplied.
  --
  -- >>> unary (UnaryR Div y) x == binary Div x y
  -- True
data UnaryR f d a = UnaryR f (Quantity d a)

instance BinaryC f d1 d2 a => UnaryC (UnaryR f d2 a) d1 a where
  type Unary (UnaryR f d2 a) d1 = Binary f d1 d2
  unary (UnaryR f y) x = binary f x y

instance BinaryC f d1 d2 a => UnaryC (UnaryL d1 a f) d2 a where
  type Unary (UnaryL d1 a f) d2 = Binary f d1 d2
  unary (UnaryL x f) y = binary f x y

-- |
-- >>> binary Div x y == x / y
-- True
data Div = Div

instance Fractional a => BinaryC Div d1 d2 a where
  type Binary Div d1 d2 = d1 / d2
  binary Div x y = x / y

-- |
-- >>> binary Mul _2 (4.0 *~ meter)
-- 8.0 m
-- >>> unary (UnaryR Mul (_2::Dimensionless Double)) (4.0 *~ meter::Length Double)
-- 8.0 m
data Mul = Mul

instance Num a => BinaryC Mul d1 d2 a where
  type Binary Mul d1 d2 = d1 * d2
  binary Mul x y = x * y


-- |
  -- >>> unary Neg x == negate x
  -- True
data Neg = Neg

instance Num a => UnaryC Neg d a where
  type Unary Neg d = d
  unary Neg = negate

-- |
  -- >>> unary Rec x == x ^ neg1
  -- True
data Rec = Rec

instance Fractional a => UnaryC Rec d a where
  type Unary Rec d = Recip d
  unary Rec x = _1 / x

-- |
  -- >>> unary Id x == x
  -- True
data Id = Id

instance Num a => UnaryC Id d a where
  type Unary Id d = d
  unary Id = id


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
scaleVec' :: (f ~ UnaryR Mul d a, VMapC f ds a, Fractional a)
          => Quantity d a -> Vec ds a -> Vec (ScaleVec d ds a) a
          -- => Quantity d a -> Vec ds a -> Vec (VMap f ds) a
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


-- Elementwise multiplication of vectors
-- -------------------------------------
-- | Multiplies each element i of the first argument vector by the
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


-- Elementwise division of vectors
-- -------------------------------
-- | Divides each element i of the first argument vector by the
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
class CrossProductC (ds1::[Dimension]) (ds2::[Dimension]) where
  type CrossProduct ds1 ds2 :: [Dimension]
  crossProduct :: Num a => Vec ds1 a -> Vec ds2 a -> Vec (CrossProduct ds1 ds2) a

instance ((b*f) ~ (e*c), (c*d) ~ (a*f), (a*e) ~ (d*b))
  => CrossProductC [a,b,c] [d,e,f] where
  type CrossProduct [a,b,c] [d,e,f] = [b * f, c * d, a * e]
  crossProduct (ListVec [a,b,c]) (ListVec [d,e,f]) = ListVec
    [ b P.* f P.- e P.* c
    , c P.* d P.- f P.* a
    , a P.* e P.- d P.* b
    ]

-- | Principled implementation of 'crossProduct'.
  --
  -- >>> crossProduct vh1 vh1 == crossProduct' vh1 vh1
  -- True
  -- >>> crossProduct vc3 vc4 == crossProduct' vc3 vc4
  -- True
crossProduct' v1 v2 =  (b * f - e * c)
                   <:  (c * d - f * a)
                   <:. (a * e - d * b)
  where 
    a = vElemAt nat0 v1
    b = vElemAt nat1 v1
    c = vElemAt nat2 v1
    d = vElemAt nat0 v2
    e = vElemAt nat1 v2
    f = vElemAt nat2 v2

-- ---------------------------------

nat0 = Proxy :: Proxy 0
nat1 = Proxy :: Proxy 1
nat2 = Proxy :: Proxy 2
nat3 = Proxy :: Proxy 3

test = let
    x = 2 *~ meter
    y = 3 *~ kilo gram
    z = _1
    v = x <: (y <:. z)
    v' = (2 *~ meter) <: ((3 *~ kilo gram) <:. _1)
  in do
  print $ x == vElemAt nat0 v
  print $ y == vElemAt nat1 v
  print $ z == vElemAt nat2 v
  print $ v == v'
