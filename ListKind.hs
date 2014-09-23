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
