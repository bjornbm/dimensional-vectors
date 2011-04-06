> {-# OPTIONS_GHC -fglasgow-exts #-}
> {-# LANGUAGE UndecidableInstances #-}

> module Matrix where

> import Data.List (intercalate)
> import Data.HList
> import MyHList
> import Numeric.Units.Dimensional (Dimensional (..), Quantity, Mul)
> import Numeric.Units.Dimensional.Prelude
> import qualified Prelude as P
> import qualified Orthogonals as O
> import Vector


| A matrix is a list of rows (which in turn are lists). The matrix construction
functions available (i.e. 'consRow') guarantee that matrices are well-formed
(each row has the same number of elements). That a matrix is a list of rows as
opposed to a list of columns is an implementation detail that we try to not leak
through the API. Ultimately, however, that choice will be visible through the
type signatures of matrices.

> data Mat vs a = ListMat [[a]] -- deriving Show


Showing
-------
A custom @show@ instance for debugging purposes.

> data ShowElems = ShowElems
> instance HMapOut ShowElem l String => Apply ShowElems l [String]
>   where apply _ = hMapOut ShowElem

> instance (RowHLists (Mat vs a) ls, HMapOut ShowElems ls [String])
>   => Show (Mat vs a) where
>   show = (\s -> "<< " ++ s ++ " >>")
>        . intercalate " >,\n < "
>        . map (intercalate ", ")
>        . hMapOut ShowElems
>        . toRowHLists


Rows and colums
---------------
Class for constraining the number of rows in a matrix. Does not ensure that the
matrix is wellformed.

> --class Rows vs n | vs -> n
> --instance HLength vs n => Rows vs n

Class for constraining the number of columns in a matrix. In particular ensures
that a matrix is well-formed.

> class Cols vs n | vs -> n
> instance Cols HNil n  -- I'm surprised this is consistent with above FD!
> instance (HLength v n, Cols vs n) => Cols (v:*:vs) n


Matrix construction
===================
| Convert ("promote") a vector to a row matrix.

> rowMatrix :: Vec ds a -> Mat (HSing ds) a
> rowMatrix (ListVec xs) = ListMat [xs]

| Convert ("promote") a vector to a column matrix.

> colMatrix :: HMap Sing ds vs => Vec ds a -> Mat vs a
> colMatrix (ListVec xs) = ListMat (map (:[]) xs)

The @Cols vs n@ constraint above shouldn't be strictly necessary assuming all
matrices are produced using 'consRow' but I believe it does no harm either and
will at least prevent building upon a malformed matrix.

| Prepends a row to a matrix.

> consRow :: Cols (v:*:vs) n => Vec v a -> Mat vs a -> Mat (v:*:vs) a
> consRow (ListVec v) (ListMat vs) = ListMat (v:vs)

> consCol :: Apply ConsEach (xs, vs) vs' => Vec xs a -> Mat vs a -> Mat vs' a
> consCol (ListVec xs) (ListMat vs) = ListMat (zipWith (:) xs vs)


Convert to/from HLists
----------------------
This class allows converting a matrix to an equivalent HList of HLists (each representing one row in the matrix) or from a well-formed HList of HLists into a matrix.

Properties:
  fromRowHLists . toRowHLists = id
  toRowHLists . fromRowHLists = id

TODO: The @HNil@ instance should be removed -- we do not want to allow creation
of empty matrices.

> class RowHLists m l | m -> l, l -> m where
>     toRowHLists   :: m -> l
>     fromRowHLists :: l -> m

> instance RowHLists (Mat HNil a) HNil where
>     toRowHLists   _ = HNil
>     fromRowHLists _ = ListMat []

> instance (VHList (Vec v a) l, RowHLists (Mat vs a) ls, Cols (v:*:vs) n)
>       => RowHLists (Mat (v:*:vs) a) (l:*:ls)
>   where
>     toRowHLists m = HCons (toHList (rowHead m)) (toRowHLists (rowTail m))
>     fromRowHLists (HCons l ls) = consRow (fromHList l) (fromRowHLists ls)


Head and tail
-------------
| Return the first row of the matrix.

> rowHead :: Mat (v:*:vs) a -> Vec v a
> rowHead (ListMat vs) = ListVec (head vs)

| Drop the first row of the matrix.
TODO: The @HNil@ instance should be removed -- we do not want to allow creation
of empty matrices.

> rowTail :: Mat (v:*:vs) a -> Mat vs a
> rowTail (ListMat vs) = ListMat (tail vs)


Transpose
=========
Thanks to the @Apply ConsCol@ instances the 'Transpose' instance is pretty simple!
TODO: @transpose mEmpty@ crashes!

> class Transpose vs vs' | vs -> vs' where
>   transpose :: Mat vs a -> Mat vs' a
>   transpose (ListMat []) = ListMat []
>   transpose (ListMat vs) = ListMat (O.transposed vs)
> instance (HHead m v, HMap HNil v v', HFoldr ConsEach v' m m') => Transpose m m'


Matrix times vector
===================
Multiplying a matrix by a vector. I believe there is some pretty
term that could be used... project??

> class MatrixVector vs v v' | vs v -> v' where
>   matVec :: Num a => Mat vs a -> Vec v a -> Vec v' a
>   matVec (ListMat vs) (ListVec v) = ListVec (O.matrix_ket vs v)

> data DotProd
> instance DotProduct v1 v2 v3 => Apply  DotProd (v2, v1) v3 where apply _ _ = undefined
> instance DotProduct v1 v2 v3 => Apply (DotProd, v2) v1  v3 where apply _ _ = undefined
> instance HMap (DotProd, v) m m' => MatrixVector m v m'


Matrix time matrix
==================
Multiplication of two matrices.

> class MatrixMatrix m1 m2 m3 | m1 m2 -> m3 where
>   matMat :: Num a => Mat m1 a -> Mat m2 a -> Mat m3 a
>   matMat (ListMat m) (ListMat m') = ListMat (O.matrix_matrix m (O.transposed m'))

> data MatVec
> instance MatrixVector m v v' => Apply  MatVec (m, v) v' where apply _ _ = undefined
> instance MatrixVector m v v' => Apply (MatVec, m) v  v' where apply _ _ = undefined
> instance (Transpose m2 m2', HMap (MatVec, m1) m2' m3', Transpose m3' m3)
>   => MatrixMatrix m1 m2 m3


Miscellaneous
=============
Scale a matrix (multiply by a scalar).

> data ScaleV
> instance HMap (MulD, d) ds1 ds2 => Apply  ScaleV (d, ds1) ds2 where apply _ = undefined
> instance HMap (MulD, d) ds1 ds2 => Apply (ScaleV, d) ds1  ds2 where apply _ = undefined

> scaleMat :: (HMap (ScaleV, d) vs1 vs2, Num a) => Quantity d a -> Mat vs1 a -> Mat vs2 a
> scaleMat (Dimensional x) (ListMat vs) = ListMat (fmap (fmap (x P.*)) vs)

Addition and subtraction of matrices.

> mElemAdd :: Num a => Mat vs a -> Mat vs a -> Mat vs a
> mElemAdd (ListMat vs1) (ListMat vs2) = ListMat (zipWith (zipWith (P.+)) vs1 vs2)

> mElemSub :: Num a => Mat vs a -> Mat vs a -> Mat vs a
> mElemSub (ListMat vs1) (ListMat vs2) = ListMat (zipWith (zipWith (P.-)) vs1 vs2)


> ex (ListMat vs) = vs


Rotation matrices (cartesian)
-----------------------------
Convenience type for homogeneous 3x3 matrices.

> type Homo33 d = Mat ((d:*:d:*.d) :*:
>                      (d:*:d:*.d) :*.
>                      (d:*:d:*.d))

> type Homo3 d = Vec (d:*:d:*.d)

> x,y,z :: Num a => Homo3 DOne a
> x = vCons _1 $ vCons _0 $ vSing _0
> y = vCons _0 $ vCons _1 $ vSing _0
> z = vCons _0 $ vCons _0 $ vSing _1

Rotation matrices. Rotates a vector by the given angle (analogous to rotating the coordinate system in opposite direction).

> rotX :: Floating a => PlaneAngle a -> Homo33 DOne a
> rotX a = consRow   (vCons _1 $ vCons _0      $ vSing _0)
>        $ consRow   (vCons _0 $ vCons (cos a) $ vSing (negate (sin a)))
>        $ rowMatrix (vCons _0 $ vCons (sin a) $ vSing (cos a))

> rotY :: Floating a => PlaneAngle a -> Homo33 DOne a
> rotY a = consRow   (vCons (cos a)          $ vCons _0 $ vSing (sin a))
>        $ consRow   (vCons _0               $ vCons _1 $ vSing _0)
>        $ rowMatrix (vCons (negate (sin a)) $ vCons _0 $ vSing (cos a))

> rotZ :: Floating a => PlaneAngle a -> Homo33 DOne a
> rotZ a = consRow   (vCons (cos a) $ vCons (negate (sin a)) $ vSing _0)
>        $ consRow   (vCons (sin a) $ vCons (cos a)          $ vSing _0)
>        $ rowMatrix (vCons _0      $ vCons _0               $ vSing _1)
> -- -}



Test values

> m1 = rowMatrix v1
> {-
> -- WHY DON'T THESE TYPECHECK WHEN THEY CAN BE ENTERED IN GHCi??
> m2 = consRow v2 m1 
> m3 = consRow v3 m2
> m4 = consCol v3 m2
> m5 = consRow v4 m1
> m2' = transpose m2
> m3' = transpose m3
> -- -}

> m6 = fromRowHLists ((1.1 *~ meter .*. 2 *~ second .*. HNil)
>                 .*. (3.3 *~ meter .*. 1 *~ second .*. HNil)
>                 .*. HNil)
> m7 = fromRowHLists ((1.1 *~ second .*. 2 *~ meter .*. HNil)
>                 .*. (3.3 *~ second .*. 1 *~ meter .*. HNil)
>                 .*. HNil)
> m8 = fromRowHLists ((1.1 *~ second .*. 2 *~ second .*. HNil)
>                 .*. (3.3 *~ meter  .*. 1 *~ meter  .*. HNil)
>                 .*. HNil)
> m6m8 = matMat m6 m8
> m8m6 = matMat m8 m6

> mm1 = rowMatrix $ fromHList $ _1 .*. 2 *~ second .*. HNil
> mm2 = fromRowHLists 
>   $   (1 *~ meter ^ pos2            .*. 2 *~ (meter ^ pos2 / second)        .*. HNil)
>   .*. (3 *~ (meter ^ pos2 / second) .*. 4 *~ (meter ^ pos2 / second ^ pos2) .*. HNil)
>   .*. HNil
> vv1 = fromHList (0 *~ meter ^ pos2 .*. 0 *~ (meter ^ pos2 / second) .*. HNil)
