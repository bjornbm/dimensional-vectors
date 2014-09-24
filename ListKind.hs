{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
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

module ListKind where

import GHC.TypeLits


type family Append (ds1::[k]) (ds2::[k]) :: [k]
  where
    Append '[d]       ds  = d ': ds
    Append (d ': ds1) ds2 = d ': Append ds1 ds2

type Snoc ds d = Append ds '[d]



-- Deconstruction
-- ==============

-- List style deconstruction (head, tail)
-- --------------------------------------

type family Head (ds::[k]) :: k
  where Head (d ': ds) = d

-- \ Non-empty tail.
type family Tail (ds::[k]) :: [k]
  where Tail (d1 ': d2 ': ds) = d2 ': ds


-- Dlist style (last, init)
-- ------------------------

type family Last (ds::[k]) :: k where
  Last '[d] = d
  Last (d ': ds) = Last ds

-- | Non-empty init.
type family Init (ds::[k]) :: [k] where
  Init '[d1, d2] = '[d1]
  Init (d1 ': d2 ': ds) = d1 ': Init (d2 ': ds)


-- Element lookup
-- --------------

type family ElemAt (n::Nat) (ds::[k]) :: k
  where
    ElemAt 0 (d ': ds) = d
    ElemAt n (d ': ds) = ElemAt (n - 1) ds



-- Homogeneous lists
-- =================

type family Homo (ds::[k]) :: k where
  Homo '[d] = d
  Homo (d ': d ': ds) = Homo (d ': ds)


-- Miscellaneous
-- =============

-- Length
-- ------

-- TODO fix naming (conflicts with Length in Dimensional). Perhaps rename Elements?
-- | Elements == length of the list.
type family Elements (ds::[k]) :: Nat
  where
    Elements '[d] = 1
    Elements (d ': ds) = Elements ds + 1


-- Forth style rot
-- ---------------

type Rot ds = Snoc (Tail ds) (Head ds)



-- Matrices (lists of lists)
-- =========================

-- We arbitrarily pick the convention that matrices are a list of
-- rows (as opposed to columns which would also be a valid choice).
-- The below examples illustrate the chosen layout.
--
-- A single row matrix:
--
--    [[x,y,z]]
--
-- A single column matrix:
--
--    [ [x]
--    , [y]
--    , [z]
--    ]
--
-- A 3x3 matrix:
--
--   [ [x,y,z]
--   , [t,y,v]
--   , [q,r,s]
--   ]
--


-- | Conversion from a list to a single row matrix.
type Row (ds::[k]) = '[ds]  -- For symmetry.

-- | Addition of a row to the top of a matrix.
type ConsRow v (vs::[k]) = v ': vs  -- For symmetry.

-- | Appending rows to the bottom of a matrix.
type AppendRows vs1 vs2 = Append vs1 vs2

-- | The top row of a matrix as a list.
type HeadRow  (vs::[[k]]) = Head vs

-- | Dropping the top row of a matrix.
type TailRows (vs::[[k]]) = Tail vs


-- | Conversion from a list to a single column matrix.
type family Column ds where
  Column (d ': '[]) = '[d] ': '[]
  Column (d ': ds)  = '[d] ': Column ds

-- | Addition of a row to the left of a matrix.
type family ConsCol ds vs where
  ConsCol '[d] '[v] = '[d ': v]
  ConsCol (d ': ds) (v ': vs) = (d ': v) ': ConsCol ds vs

-- | Appending columns to the right of a matrix.
type AppendCols vs1 vs2 = Transpose (Append (Transpose vs1) (Transpose vs2))

-- | The leftmost column of a matrix as a list.
type HeadCol  vs = HeadRow  (Transpose vs)

-- | Dropping the leftmost column of a matrix.
type TailCols vs = Transpose (TailRows (Transpose vs))

-- | Transposing a matrix (x_ij -> x_ji).
type family Transpose (vs::[[k]]) :: [[k]] where
  Transpose '[v] = Column v
  Transpose (v ': vs) = ConsCol v (Transpose vs)
