{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module VApply where

import Numeric.Units.Dimensional.DK.Prelude
import Apply
import ListKind
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


-- Operations from vectors to quantities
-- =====================================

-- Unary operations
-- ----------------

class VUnaryQC f ds a where
  type VUnaryQ f ds :: Dimension
  -- | Apply a function from a vector to a quantity.
  vUnaryQ :: f -> Vec ds a -> Quantity (VUnaryQ f ds) a


-- | Sum of all elements.
  --
  -- >>> vUnaryQ Sum vh1 == vSum vh1
  -- True
data Sum = Sum

instance Num a => VUnaryQC Sum ds a where
  type VUnaryQ Sum ds = Homo ds
  vUnaryQ Sum = vSum

-- Instance for functions.
instance VUnaryQC (Vec ds a -> Quantity d a) ds a where
  type VUnaryQ (Vec ds a -> Quantity d a) ds = d
  vUnaryQ f = f


-- Binary operations
-- -----------------

class VBinaryQC f v1 v2 a where
  type VBinaryQ f v1 v2 :: Dimension
  -- | Apply a binary operation to two quantities.
  vBinaryQ :: f -> Vec v1 a -> Vec v2 a -> Quantity (VBinaryQ f v1 v2) a

-- | Dot product.
  --
  -- >>> vBinaryQ Dot v vd2 == dotProduct v vd2
  -- True
data Dot = Dot

instance Num a => VBinaryQC Dot v1 v2 a where
  type VBinaryQ Dot v1 v2 = DotProduct v1 v2
  vBinaryQ Dot x y = dotProduct x y


-- Operations from vectors to vectors
-- ==================================

-- Unary operations
-- ----------------

-- Vector to vector.
class VUnaryVC f ds a where
  type VUnaryV f ds :: [Dimension]
  -- | Apply a function from a vector to a vector.
    --
    -- >>> vUnaryV Id v == v
    -- True
  vUnaryV :: f -> Vec ds a -> Vec (VUnaryV f ds) a

-- Example instance for Id.
instance Num a => VUnaryVC Id ds a where
  type VUnaryV Id ds = ds
  vUnaryV Id = id

-- Instance for functions.
instance VUnaryVC (Vec ds1 a -> Vec ds2 a) ds1 a where
  type VUnaryV (Vec ds1 a -> Vec ds2 a) ds1 = ds2
  vUnaryV f = f


-- Binary operations
-- -----------------

class VBinaryVC f v1 v2 a where
  type VBinaryV f v1 v2 :: [Dimension]
  -- | Apply a binary operation to two quantities.
    --
    -- >>> vBinaryV Add v v == elemAdd v v
    -- True
  vBinaryV :: f -> Vec v1 a -> Vec v2 a -> Vec (VBinaryV f v1 v2) a

-- Example instance for addition.
instance Num a => VBinaryVC Add v v a where
  type VBinaryV Add v v = v
  vBinaryV Add = elemAdd

-- | Const function.
  --
  -- vBinaryV Const v vc3 == v
  -- True
data Const = Const
instance VBinaryVC Const v1 v2 a where
  type VBinaryV Const v1 v2 = v1
  vBinaryV Const = const



-- Binary to unary conversion
-- ==========================

-- | Type for making a binary operation unary, with the left argument
  -- pre-supplied.
  --
  -- >>> vUnaryQ (VUnaryL v Dot) vd2 == vBinaryQ Dot v vd2
  -- True
  -- >>> vUnaryV (VUnaryL v Const) vd2 == vBinaryV Const v vd2
  -- True
data VUnaryL ds a f = VUnaryL (Vec ds a) f

instance VBinaryQC f ds1 ds2 a => VUnaryQC (VUnaryL ds1 a f) ds2 a where
  type VUnaryQ (VUnaryL ds1 a f) ds2 = VBinaryQ f ds1 ds2
  vUnaryQ (VUnaryL v1 f) v2 = vBinaryQ f v1 v2

instance VBinaryVC f ds1 ds2 a => VUnaryVC (VUnaryL ds1 a f) ds2 a where
  type VUnaryV (VUnaryL ds1 a f) ds2 = VBinaryV f ds1 ds2
  vUnaryV (VUnaryL v1 f) v2 = vBinaryV f v1 v2


-- | Type for making a binary operation unary, with the right argument
  -- pre-supplied.
  --
  -- >>> vUnaryQ (VUnaryR Dot v) vd2 == vBinaryQ Dot vd2 v
  -- True
  -- >>> vUnaryV (VUnaryR Const v) vd2 == vBinaryV Const vd2 v
  -- True
data VUnaryR f ds a = VUnaryR f (Vec ds a)

instance VBinaryQC f ds1 ds2 a => VUnaryQC (VUnaryR f ds2 a) ds1 a where
  type VUnaryQ (VUnaryR f ds2 a) ds1 = VBinaryQ f ds1 ds2
  vUnaryQ (VUnaryR f v2) v1 = vBinaryQ f v1 v2

instance VBinaryVC f ds1 ds2 a => VUnaryVC (VUnaryR f ds2 a) ds1 a where
  type VUnaryV (VUnaryR f ds2 a) ds1 = VBinaryV f ds1 ds2
  vUnaryV (VUnaryR f v2) v1 = vBinaryV f v1 v2



-- Vector to anything
-- ==================

class VApplyC f ds a where
  type VApply f ds a :: k  -- Has to be * for vApply to make sense?
  -- | Apply a function with arbitrary return type to a vector.
    --
    -- >>> vApply Id v == v
    -- True
    -- >>> vApply Sum vh1 == vSum vh1
    -- True
  vApply :: f -> Vec ds a -> VApply f ds a

instance VApplyC Id ds a where
  type VApply Id ds a = Vec ds a
  vApply Id = id

instance Num a => VApplyC Sum ds a where
  type VApply Sum ds a = Quantity (Homo ds) a
  vApply Sum = vSum
