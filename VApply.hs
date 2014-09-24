{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
class VApplyQC f ds a where
  type VApplyQ f ds :: Dimension
  -- | Apply a function from a vector to a quantity.
    --
    -- >>> vApplyVQ Sum vh1 == vSum vh1
    -- True
  vApplyQ :: f -> Vec ds a -> Quantity (VApplyQ f ds) a

-- Example instance for Sum.
instance Num a => VApplyQC Sum ds a where
  type VApplyQ Sum ds = Homo ds
  vApplyQ Sum = vSum

-- Instance for functions.
instance VApplyQC (Vec ds a -> Quantity d a) ds a where
  type VApplyQ (Vec ds a -> Quantity d a) ds = d
  vApplyQ f = f





-- Vector to vector.
class VApplyVC f ds a where
  type VApplyV f ds :: [Dimension]
  -- | Apply a function from a vector to a vector.
    --
    -- >>> vApplyV Id v == v
    -- True
  vApplyV :: f -> Vec ds a -> Vec (VApplyV f ds) a

-- Example instance for Id.
instance Num a => VApplyVC Id ds a where
  type VApplyV Id ds = ds
  vApplyV Id = id

-- Instance for functions.
instance VApplyVC (Vec ds1 a -> Vec ds2 a) ds1 a where
  type VApplyV (Vec ds1 a -> Vec ds2 a) ds1 = ds2
  vApplyV f = f



