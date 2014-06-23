{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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
-- >>> let x = 2 *~ meter
-- >>> let y = 3 *~ kilo gram
-- >>> let z = _1
-- >>> let v = x <: (y <:. z)
-- >>> let v' = (2 *~ meter) <: ((3 *~ kilo gram) <:. _1)

infixr 5  <:, <:.


data Vec' :: [Dimension] -> * -> * where
  VSing :: Quantity d a -> Vec' '[d] a
  (:>)  :: Quantity d a -> Vec' ds a -> Vec' (d ': ds) a

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

instance BinaryC f d1 d2 a => UnaryC (UnaryR f d2 a) d1 a where
  type Unary (UnaryR f d2 a) d1 = Binary f d1 d2
  unary (UnaryR f y) x = binary f x y

instance BinaryC f d1 d2 a => UnaryC (UnaryL d1 a f) d2 a where
  type Unary (UnaryL d1 a f) d2 = Binary f d1 d2
  unary (UnaryL x f) y = binary f x y

data UnaryL d a f = UnaryL (Quantity d a) f
data UnaryR f d a = UnaryR f (Quantity d a)

-- >>> unary (Div, _2::Dimensionless Double) (4.0 *~ meter::Length Double)
-- 2.0 m
data Div = Div
{-
instance (d2 ~ (d1 / d), Fractional a) => UnaryC (Div, Quantity d a) d1 a
  where
    type Unary (Div, Quantity d a) d1 = d1 / d
    unary (Div, y) x = x / y
    -}

-- |
-- >>> binary Mul _2 (4.0 *~ meter)
-- 8.0 m
-- >>> unary (UnaryR Mul (_2::Dimensionless Double)) (4.0 *~ meter::Length Double)
-- 8.0 m
data Mul = Mul
{-
instance (d2 ~ (d1 * d), Num a) => UnaryC (Mul, Quantity d a) d1 a
  where
    type Unary (Mul, Quantity d a) d1 = d1 * d
    unary (Mul, y) x = x * y
    -}
instance Num a => BinaryC Mul d1 d2 a
  where
    type Binary Mul d1 d2 = d1 * d2
    binary Mul x y = x * y

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
scaleVec :: (f ~ UnaryR Mul d a, VMap f ds a, Fractional a)
         => Quantity d a -> Vec ds a -> Vec (VMap' f ds) a
--scaleVec :: (VMap (Mul, Quantity d a) ds a, Fractional a)
--         => Quantity d a -> Vec ds a -> Vec (VMap' (Mul, Quantity d a) ds) a
scaleVec x = vMap (UnaryR Mul x)

-- | Scale a vector by a dimensionless quantity. This avoids the trivial
-- constraint @HMap (MulD, DOne) ds ds@ for this common case.
--
-- >>> scaleVec1 _2 (_4 <:. 3 *~ meter)
-- < 8.0, 6.0 m >
scaleVec1 :: Fractional a => Dimensionless a -> Vec ds a -> Vec ds a
scaleVec1 x (ListVec xs) = ListVec $ map ((x /~ one) P.*) xs



class VZipWith f ds1 ds2 where
  type VZW f ds1 ds2 :: [Dimension]
  vZipWith :: f -> Vec ds1 a -> Vec ds2 a -> Vec (VZW f ds1 ds2) a

{-
instance VZipWith f '[d1] '[d2] where
  type VZW f '[d1] '[d2] = '[Unary f (d1,d2)]
  vZipWith f v1 v2 = unary f (vHead v1, vHead v2)
-}


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
