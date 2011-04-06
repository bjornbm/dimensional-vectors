{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}


module MyHList where

import Data.HList
import Numeric.NumType (PosType, Zero, Pos)

-- | Shorthand type operator for the last element in a HList type
type a :*. b = a :*: b :*: HNil
-- | Type synonym for a Singleton HList.
type HSing a = HCons a HNil

-- | This class is a candidate for the HList library I would think.
class HZipWith f l1 l2 l3 where hZipWith :: f -> l1 -> l2 -> l3
instance (HZip l1 l2 l', HMap f l' l3) => HZipWith f l1 l2 l3
  where hZipWith f l1 l2 = hMap f (hZip l1 l2)

-- | The @Apply MkNil@ is used to convert anything into an empty list.
-- Use with e.g. @HMap@ on a list of stuff to create an empty list.
instance Apply HNil a HNil where apply _ _ = HNil

-- | The @Apply Cons@ instance converts a pair into an HList. The second element of
-- the pair must be an HList.
data Cons = Cons
instance HList l => Apply Cons (e, l) (e:*:l) where apply _ = uncurry HCons
instance HList l => Apply (Cons,l) e (e:*:l) where apply (_,l) e = HCons e l

-- | The Apply instance producing a singleton HList.
type Sing = (Cons,HNil)

-- | @Apply ConsEach@ takes a pair of a HList (the heads) and a HList of HLists
-- (the tails). The number of heads must be identical to the number of tails.
-- The heads are prepended to the tails, one head per tail. The end result is an
-- HList of HLists with one additional element each.
data ConsEach
instance HZipWith Cons xs vs vs' => Apply ConsEach (xs, vs) vs'
  where apply _ = uncurry (hZipWith Cons)


-- | Class for conversion from HList's $HNat$s to NumTypes $NumType$s.
class (HNat h, PosType n) => HNatNumType h n | h -> n, n -> h where
  fromHNat :: h -> n
  fromHNat _ = undefined
  toHNat :: n -> h
  toHNat _ = undefined
instance HNatNumType HZero Zero
instance HNatNumType h n => HNatNumType (HSucc h) (Pos n)

