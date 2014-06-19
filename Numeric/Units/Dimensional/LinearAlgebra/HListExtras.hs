{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Extra type classes and instances functions for working with HLists.
module Numeric.Units.Dimensional.LinearAlgebra.HListExtras where


import Data.HList hiding (Proxy)
import Data.Proxy
import Numeric.NumType.DK (NumType (..), Succ, Pred)

-- | Shorthand type operator for the last element in a HList type
type a :*. b = a :*: b :*: HNil
-- | Type synonym for a Singleton HList.
type HSing a = HCons a HNil


-- | Iterate a function over a HList.
class HIterate' f l a where hIterate' :: f -> l -> a
instance HIterate' a HNil a where hIterate' = const
instance HIterate' f l a => HIterate' (b -> f) (HCons b l) a
  where hIterate' f (HCons x l) = hIterate' (f x) l

-- | Iterate over a HList using any @Apply@ instance.
class HIterate f l a where hIterate :: f -> l -> a
instance HIterate a HNil a where hIterate = const
instance (Apply g b f, HIterate f l a) => HIterate g (HCons b l) a
  where hIterate f (HCons x l) = hIterate (apply f x) l

-- | This class is a candidate for the HList library I would think.
class HZipWith f l1 l2 l3 | f l1 l2 -> l3 where hZipWith :: f -> l1 -> l2 -> l3
instance (HZip l1 l2 l', HMap f l' l3) => HZipWith f l1 l2 l3
  where hZipWith f l1 l2 = hMap f (hZip l1 l2)

-- | The @Apply MkNil@ is used to convert anything into an empty list.
-- Use with e.g. @HMap@ on a list of stuff to create an empty list.
instance Apply HNil a HNil where apply _ _ = HNil

-- | The @Apply Cons@ instance converts a pair into an HList. The second element of
-- the pair must be an HList.
data Cons = Cons
instance HList l => Apply Cons (e,l) (e:*:l) where apply _ = uncurry HCons
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


-- | Conversion from NumType's @NumType@s to HList's @HNat@s.
type family AsHNat (n::NumType)
  where
    AsHNat Zero = HZero
    AsHNat Pos1 = HSucc HZero
    AsHNat Pos2 = HSucc (AsHNat Pos1)
    AsHNat Pos3 = HSucc (AsHNat Pos2)
    AsHNat Pos4 = HSucc (AsHNat Pos3)
    AsHNat Pos5 = HSucc (AsHNat Pos4)
    AsHNat Pos6 = HSucc (AsHNat Pos5)
    AsHNat Pos7 = HSucc (AsHNat Pos6)
    AsHNat Pos8 = HSucc (AsHNat Pos7)
    AsHNat Pos9 = HSucc (AsHNat Pos8)
    AsHNat (Pos10Plus n) = HSucc (AsHNat (Pred (Pos10Plus n)))
-- | Conversion from NumType's @NumType@s to HList's @HNat@s.
toHNat :: Proxy (n::NumType) -> AsHNat n
toHNat _ = undefined

-- | Conversion from HList's @HNat@s to NumType's @NumType@s.
type family AsNumType n
  where
    AsNumType  HZero    = Zero
    AsNumType (HSucc n) = Succ (AsNumType n)

-- | Conversion from HList's @HNat@s to NumType's @NumType@s.
fromHNat :: n -> Proxy (AsNumType n)
fromHNat _ = undefined
