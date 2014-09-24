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



-- Example function
data Sum = Sum



-- Vector to quantity.
class VUnaryQC f ds a where
  type VUnaryQ f ds :: Dimension
  -- | Apply a function from a vector to a quantity.
    --
    -- >>> vUnaryVQ Sum vh1 == vSum vh1
    -- True
  vUnaryQ :: f -> Vec ds a -> Quantity (VUnaryQ f ds) a

-- Example instance for Sum.
instance Num a => VUnaryQC Sum ds a where
  type VUnaryQ Sum ds = Homo ds
  vUnaryQ Sum = vSum

-- Instance for functions.
instance VUnaryQC (Vec ds a -> Quantity d a) ds a where
  type VUnaryQ (Vec ds a -> Quantity d a) ds = d
  vUnaryQ f = f





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


-- ==============================

-- Vector to anything.
class VApplyC f ds a where
  type VApply f ds a :: k  -- Has to be * for vApply to make sense.
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
