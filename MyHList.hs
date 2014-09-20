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
-- >>> let v'' = x <: x <:. x
-- >>> let v''' = y <: x <:. x*y

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



--vMap :: (Apply f d a) => f -> Vec [d] a -> [App f d a]
--vMap :: f -> Vec [d] a -> [App f d a]
--vMap f v = [apply f (vHead v)]
--vMap f v = f (vHead v) : vMap f (vTail v)

class MapOut f (ds::[Dimension]) a where
  type MapO f ds a
  mapOut :: f -> Vec ds a -> [MapO f ds a]

instance (Apply f d a, Num a) => MapOut f '[d] a where
  type MapO f '[d] a = App f d a
  mapOut f v = [apply f $ vHead v]

instance ( Apply f d1 a, MapOut f (d2 ': ds) a, Num a
         , App f d1 a ~ MapO f (d2 ': ds) a)
        => MapOut f (d1 ': d2 ': ds) a
  where
    type MapO f (d1 ': d2 ': ds) a = MapO f (d2 ': ds) a -- :App f d a
    mapOut f v = apply f (vHead v) : mapOut f (vTail v)


class Apply f d a where
  type App f d a
  apply :: f -> Quantity d a -> App f d a

-- |
-- >>> applyC Show (4.2 *~ kilo meter) :: String
-- "4200.0 m"
--
-- >>> apply Show (42 *~ gram)
-- "4.2e-2 kg"
--
-- >>> show (2 *~ gram <: _3 <:. 32.3 *~ meter)
-- "< 2.0e-3 kg, 3.0, 32.3 m >"
--
data ShowQ = Show

instance (Show (Quantity d a)) => Apply ShowQ d a where
  type App ShowQ d a = String
  apply _ = show

instance Show (Quantity d a) => ApplyC ShowQ d a String where applyC _ = show

class ApplyC f d a b where applyC :: f -> Quantity d a -> b

instance MapOutC ShowQ ds a String => Show (Vec ds a)
  where show = (\s -> "< " ++ s ++ " >")
             . intercalate ", "
             . mapOutC Show


-- |
-- >>> mapOutC (undefined :: ShowQ) (vSing $ 32.3 *~ meter) :: [String]
-- ["32.3 m"]
--
-- >>> mapOutC (undefined :: ShowQ) (2 *~ gram <: _3 <:. 32.3 *~ meter) :: [String]
-- ["2.0e-3 kg","3.0","32.3 m"]
--
class MapOutC f ds a b where mapOutC :: f -> Vec ds a -> [b]

instance (ApplyC f d a b, Num a) => MapOutC f '[d] a b where
  mapOutC f v = [applyC f $ vHead v]

instance ( ApplyC f d1 a b, MapOutC f (d2 ': ds) a b, Num a
    ) => MapOutC f (d1 ': d2 ': ds) a b
  where
    mapOutC f v = applyC f (vHead v) : mapOutC f (vTail v)

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
  --
class VMap f ds1 a where
  type VMap' f ds1 :: [Dimension]
  vMap :: f -> Vec ds1 a -> Vec (VMap' f ds1) a

instance (UnaryC f d a, Fractional a) => VMap f '[d] a where
  type VMap' f '[d] = '[Unary f d]
  vMap f = vSing . unary f . vHead

instance (UnaryC f d1 a, VMap f (d2 ': ds) a, Fractional a) => VMap f (d1 ': d2 ': ds) a
  where
    type VMap' f (d1 ': d2 ': ds) = Unary f d1 ': VMap' f (d2 ': ds)
    vMap f v = unary f (vHead v) <: vMap f (vTail v)


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
scaleVec :: (f ~ UnaryR Mul d a, VMap f ds a, Fractional a)
         => Quantity d a -> Vec ds a -> Vec (VMap' f ds) a
scaleVec x v = repMap (P.* (x /~ siUnit)) v
--scaleVec x = vMap (UnaryR Mul x)  -- Rigorious implementation.

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


-- Elementwise division of vectors
-- -------------------------------
-- | Divides each element i of the first argument vector by the
  -- corresponding element of the second argument.
  --
  -- >>> elemDiv v v == vZipWith Div v v
  -- True
elemDiv :: Fractional a => Vec ds a -> Vec es a -> Vec (VZipWith Div ds es) a
elemDiv = repZipWith (P./)


-- Homogeneous vectors
-- ===================

class HomoC ds where
  type Homo ds :: Dimension
  toList :: Num a => Vec ds a -> [Quantity (Homo ds) a]
  toList (ListVec xs) = xs *~~ siUnit

instance HomoC '[d] where type Homo '[d] = d
instance (Homo ds ~ d) => HomoC (d ': ds) where type Homo (d ': ds) = d

-- | Compute the sum of all elements in a homogeneous vector.
  --
  -- >>> vSum v''
  -- 6.0 m
vSum :: (HomoC ds, Num a) => Vec ds a -> Quantity (Homo ds) a
vSum = sum . toList


-- Dot product
-- ===========

-- Type class based vector dot product. The class and associated type
-- servers to simplify constraints and type signatures of @dotProduct@.
  -- This can probably be done with a constraint synonym instead?
class DotProductC (ds1::[Dimension]) (ds2::[Dimension]) where
  type DotProduct ds1 ds2 :: Dimension
  -- | Compute the dot product of two vectors.
    --
    -- >>> dotProduct' v v'''
    -- 18.0 m kg
  dotProduct' :: Num a => Vec ds1 a -> Vec ds2 a -> Quantity (DotProduct ds1 ds2) a

instance (HomoC (VZipWith Mul ds1 ds2)) => DotProductC ds1 ds2 where
  type DotProduct ds1 ds2 = Homo (VZipWith Mul ds1 ds2)
  dotProduct' v1 v2 = vSum (elemMul v1 v2)

-- Approach using constraint synonyms.
type DotProductC' ds1 ds2 = HomoC (VZipWith Mul ds1 ds2)
type DotProduct'  ds1 ds2 = Homo  (VZipWith Mul ds1 ds2)
-- | Compute the dot product of two vectors.
  --
  -- >>> dotProduct'' v v'''
  -- 18.0 m kg
dotProduct'' :: (DotProductC' ds1 ds2, Num a)
             => Vec ds1 a -> Vec ds2 a -> Quantity (DotProduct' ds1 ds2) a
dotProduct'' v1 v2 = vSum (elemMul v1 v2)

-- | Compute the dot product of two vectors.
  --
  -- >>> dotProduct v v'''
  -- 18.0 m kg
  --
  -- This version doesn't use a custom class/type.
dotProduct :: (HomoC (VZipWith Mul ds1 ds2), Num a) =>
  Vec ds1 a -> Vec ds2 a -> Quantity (Homo (VZipWith Mul ds1 ds2)) a
dotProduct v1 v2 = vSum (elemMul v1 v2)

-- Cross Product

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
