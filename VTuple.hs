{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module VTuple (VTupleC (..), TupleVC (..)) where

import Vector
import Nats
import Numeric.Units.Dimensional.DK.Prelude
import qualified Prelude

-- $setup
-- >>> let x = 2 *~ meter :: Length Double
-- >>> let y = 3 *~ kilo gram :: Mass Double
-- >>> let z = _1 :: Dimensionless Double
-- >>> let v2 = x <:. y
-- >>> let v3 = x <: y <:. z
-- >>> let v4 = y <: x <: y <:. z
-- >>> let v5 = x <: y <: x <: y <:. z

type Q = Quantity     -- For convenience.
ve n v = vElemAt n v  -- For convenience.

-- Convert to/from Tuples
-- ======================

-- | Conversion to tuple representation. This is primarily to allow taking
-- advantage of the syntactic sugar tuples enjoy, e.g. for pattern matching.
class VTupleC (ds::[Dimension]) where
  type VTuple ds a
  -- | Convert a vector to the isomorphic tuple of quantities.
    --
    -- >>> toTuple (x <: y <:. z)
    -- (2.0 m,3.0 kg,1.0)
  toTuple :: Num a => Vec ds a -> VTuple ds a

-- | Conversion from tuple representation. This is primarily to allow taking
-- advantage of the syntactic sugar tuples enjoy.
class TupleVC t where
  type TupleV t :: [Dimension]
  type VNum t :: *
  -- | Convert a tuple of quantities to the isomorphic vector.
    --
    -- >>> fromTuple (x,y,z)
    -- < 2.0 m, 3.0 kg, 1.0 >
  fromTuple :: Fractional (VNum t) => t -> Vec (TupleV t) (VNum t)


-- Instances
-- ---------

{-
We can brute force the instances out to a reasonable degree. Presumably
syntactic sugar loses its value if the vectors get to large as it is
impractical to deal with them any way other than programmatically.
-}

-- | (,)
  -- >>> toTuple (x <:. y) == (x,y)
  -- True
  -- >>> fromTuple (x,y) == (x <:. y)
  -- True
  -- >>> fromTuple (toTuple v2) == v2
  -- True
instance VTupleC [d1,d2] where
  type   VTuple  [d1,d2] a = (Q d1 a,Q d2 a)
  toTuple v = (ve nat0 v, ve nat1 v)

instance TupleVC (Q d1 a,Q d2 a) where
  type   TupleV  (Q d1 a,Q d2 a) = '[d1,d2]
  type   VNum    (Q d1 a,Q d2 a) = a
  fromTuple (x,y) = x <:. y

-- | (,,)
  --
  -- >>> toTuple (x <: y <:. z) == (x,y,z)
  -- True
  -- >>> fromTuple (x,y,z) == (x <: y <:. z)
  -- True
  -- >>> fromTuple (toTuple v3) == v3
  -- True
instance VTupleC [d1,d2,d3] where
  type   VTuple  [d1,d2,d3] a = (Q d1 a,Q d2 a,Q d3 a)
  toTuple v = (ve nat0 v, ve nat1 v, ve nat2 v)

instance TupleVC (Q d1 a,Q d2 a,Q d3 a) where
  type   TupleV  (Q d1 a,Q d2 a,Q d3 a) = '[d1,d2,d3]
  type   VNum    (Q d1 a,Q d2 a,Q d3 a) = a
  fromTuple (x,y,z) = x <: y <:. z

-- | (,,,)
  --
  -- >>> toTuple (x <: y <: x <:. y) == (x,y,x,y)
  -- True
  -- >>> fromTuple (x,y,x,y) == (x <: y <: x <:. y)
  -- True
  -- >>> fromTuple (toTuple v4) == v4
  -- True
instance VTupleC [d1,d2,d3,d4] where
  type   VTuple  [d1,d2,d3,d4] a = (Q d1 a,Q d2 a,Q d3 a,Q d4 a)
  toTuple v = (ve nat0 v, ve nat1 v, ve nat2 v, ve nat3 v)

instance TupleVC (Q d1 a,Q d2 a,Q d3 a,Q d4 a) where
  type   TupleV  (Q d1 a,Q d2 a,Q d3 a,Q d4 a) = '[d1,d2,d3,d4]
  type   VNum    (Q d1 a,Q d2 a,Q d3 a,Q d4 a) = a
  fromTuple (v,x,y,z) = v <: x <: y <:. z

-- | (,,,)
  -- >>> toTuple (y <: x <: y <: x <:. y) == (y,x,y,x,y)
  -- True
  -- >>> fromTuple (y,x,y,x,y) == (y <: x <: y <: x <:. y)
  -- True
  -- >>> fromTuple (toTuple v5) == v5
  -- True
instance VTupleC [d1,d2,d3,d4,d5] where
  type   VTuple  [d1,d2,d3,d4,d5] a = (Q d1 a,Q d2 a,Q d3 a,Q d4 a,Q d5 a)
  toTuple v = (ve nat0 v, ve nat1 v, ve nat2 v, ve nat3 v, ve nat4 v)

instance TupleVC (Q d1 a,Q d2 a,Q d3 a,Q d4 a,Q d5 a) where
  type   TupleV  (Q d1 a,Q d2 a,Q d3 a,Q d4 a,Q d5 a) = '[d1,d2,d3,d4,d5]
  type   VNum    (Q d1 a,Q d2 a,Q d3 a,Q d4 a,Q d5 a) = a
  fromTuple (u,v,x,y,z) = u <: v <: x <: y <:. z
