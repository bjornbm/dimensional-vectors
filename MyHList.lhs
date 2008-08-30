> {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

> module MyHList where

> import HList
> import Numeric.NumType (PosType, Zero, Pos)

| This class is a candidate for the HList library I would think.

> class HZipWith f l1 l2 l3 where hZipWith :: f -> l1 -> l2 -> l3
> instance (HZip l1 l2 l', HMap f l' l3) => HZipWith f l1 l2 l3
>   where hZipWith f l1 l2 = hMap f (hZip l1 l2)

| The @Apply MkNil@ is used to convert anything into an empty list.
Use with e.g. @HMap@ on a list of stuff to create an empty list.

> data MkNil; instance Apply MkNil a HNil where apply _ _ = HNil

| The @Apply Wrap@ instance is used to convert something into a singleton list.

> data Wrap; instance Apply Wrap a (HCons a HNil) where apply _ e = HCons e HNil

| The @Apply Cons@ instance converts a pair into an HList. The second element of
the pair must be an HList.

> data Cons = Cons
> instance HList l => Apply Cons (e, l) (HCons e l) where apply _ = uncurry HCons

| @Apply ConsEach@ takes a pair of a HList (the heads) and a HList of HLists
(the tails). The number of heads must be identical to the number of tails.
The heads are prepended to the tails, one head per tail. The end result is an
HList of HLists with one additional element each.

> data ConsEach
> instance HZipWith Cons xs vs vs' => Apply ConsEach (xs, vs) vs'
>   where apply _ = uncurry (hZipWith Cons)



> class (HNat h, PosType n) => HNatNumType h n | h -> n, n -> h where
>   fromHNat :: h -> n
>   fromHNat _ = undefined
>   toHNat :: n -> h
>   toHNat _ = undefined
> instance HNatNumType HZero Zero
> instance HNatNumType h n => HNatNumType (HSucc h) (Pos n)

