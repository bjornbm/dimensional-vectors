{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Apply where

import Numeric.Units.Dimensional.DK.Prelude
import qualified Prelude

-- $setup
-- >>> let x = 2 *~ meter :: Length Double
-- >>> let y = 3 *~ kilo gram :: Mass Double


-- Operations from quantities to quantities
-- ========================================

-- Unary operations
-- ----------------

class UnaryC f d a where
  type Unary f d :: Dimension
  -- | Apply a unary operation to a quantity.
  unary :: f -> Quantity d a -> Quantity (Unary f d) a


-- | The identity operation.
  --
  -- >>> unary Id x == x
  -- True
data Id = Id

instance Num a => UnaryC Id d a where
  type Unary Id d = d
  unary Id = id


-- | Negation operation.
  --
  -- >>> unary Neg x == negate x
  -- True
data Neg = Neg

instance Num a => UnaryC Neg d a where
  type Unary Neg d = d
  unary Neg = negate

-- | Reciprocation operation.
  --
  -- >>> unary Rec x == x ^ neg1
  -- True
data Rec = Rec

instance Fractional a => UnaryC Rec d a where
  type Unary Rec d = Recip d
  unary Rec x = _1 / x


-- Binary operations
-- -----------------

class BinaryC f d1 d2 a where
  type Binary f d1 d2 :: Dimension
  -- | Apply a binary operation to two quantities.
  binary :: f -> Quantity d1 a -> Quantity d2 a -> Quantity (Binary f d1 d2) a


-- | Addition operation.
  --
  -- >>> binary Add (2 *~ meter) (4.0 *~ meter)
  -- 6.0 m
data Add = Add

instance Num a => BinaryC Add d d a where
  type Binary Add d d = d
  binary Add x y = x + y

-- | Subtraction operation.
  --
  -- >>> binary Sub (2 *~ meter) (4.0 *~ meter)
  -- -2.0 m
data Sub = Sub

instance Num a => BinaryC Sub d d a where
  type Binary Sub d d = d
  binary Sub x y = x - y

-- | Division operation.
  --
  -- >>> binary Div x y == x / y
  -- True
data Div = Div

instance Fractional a => BinaryC Div d1 d2 a where
  type Binary Div d1 d2 = d1 / d2
  binary Div x y = x / y

-- | Multiplication operation.
  --
  -- >>> binary Mul _2 (4.0 *~ meter)
  -- 8.0 m
  -- >>> unary (UnaryR Mul (_2::Dimensionless Double)) (4.0 *~ meter::Length Double)
  -- 8.0 m
data Mul = Mul

instance Num a => BinaryC Mul d1 d2 a where
  type Binary Mul d1 d2 = d1 * d2
  binary Mul x y = x * y


-- Binary to unary conversion
-- --------------------------

-- | Type for making a binary operation unary, with the left argument
  -- pre-supplied.
  --
  -- >>> unary (UnaryL x Div) y == binary Div x y
  -- True
data UnaryL d a f = UnaryL (Quantity d a) f

instance BinaryC f d1 d2 a => UnaryC (UnaryL d1 a f) d2 a where
  type Unary (UnaryL d1 a f) d2 = Binary f d1 d2
  unary (UnaryL x f) y = binary f x y


-- | Type for making a binary operation unary, with the right argument
  -- pre-supplied.
  --
  -- >>> unary (UnaryR Div y) x == binary Div x y
  -- True
data UnaryR f d a = UnaryR f (Quantity d a)

instance BinaryC f d1 d2 a => UnaryC (UnaryR f d2 a) d1 a where
  type Unary (UnaryR f d2 a) d1 = Binary f d1 d2
  unary (UnaryR f y) x = binary f x y



-- Operations from quantities to other types
-- =========================================

class ApplyC f d a where
  type Apply f d a
  -- | Apply an operation from a quantity to an arbitrary type.
  apply :: f -> Quantity d a -> Apply f d a


-- | Wrap an operation implementing 'UnaryC' with so it can be used
  -- with 'apply'.
  --
  -- >>> apply (Un Id) x == x
  -- True
  -- >>> apply (Un Neg) x == unary Neg x
  -- True
  -- >>> apply (Un Rec) x == unary Rec x
  -- True
  -- >>> apply (Un (UnaryR Div y)) x == binary Div x y
  -- True
  -- >>> apply (Un (UnaryL x Div)) y == binary Div x y
  -- True
data Un f = Un f

instance UnaryC f d a => ApplyC (Un f) d a where
  type Apply (Un f) d a = Quantity (Unary f d) a
  apply (Un f) = unary f

{-
instance ApplyC Id d a where
  type Apply Id d a = Quantity d a
  apply Id = id
  -}


-- | The show operation.
  --
  -- >>> apply Show (4.2 *~ kilo meter) :: String
  -- "4200.0 m"
  -- >>> apply Show (42 *~ gram)
  -- "4.2e-2 kg"
data Show' = Show

instance Show (Quantity d a) => ApplyC Show' d a where
  type Apply Show' d a = String
  apply Show = show
