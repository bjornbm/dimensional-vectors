> {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

> module Matrix where

> import Data.List (intercalate)
> import HList
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

> instance (ToRowHLists (Mat vs a) ls, HMapOut ShowElems ls [String])
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
> instance Cols HNil n
> instance (HLength v n, Cols vs n) => Cols (HCons v vs) n


Matrix construction
===================
| Convert ("promote") a vector to a row matrix.

> rowMatrix :: Vec ds a -> Mat (HCons ds HNil) a
> rowMatrix (ListVec xs) = ListMat [xs]

| Convert ("promote") a vector to a column matrix.

> colMatrix :: HMap Wrap ds vs => Vec ds a -> Mat vs a
> colMatrix (ListVec xs) = ListMat (map (:[]) xs)

The @Cols vs n@ constraint above shouldn't be strictly necessary assuming all
matrices are produced using 'consRow' but I believe it does no harm either and
will at least prevent building upon a malformed matrix.

| Prepends a row to a matrix.

> consRow :: (HLength xs n, Cols vs n) => Vec xs a -> Mat vs a -> Mat (HCons xs vs) a
> consRow (ListVec xs) (ListMat vs) = ListMat (xs:vs)

> consCol :: Apply ConsEach (xs, vs) vs' => Vec xs a -> Mat vs a -> Mat vs' a
> consCol (ListVec xs) (ListMat vs) = ListMat (zipWith (:) xs vs)

This class allows converting a matrix to an equivalent HList of HLists,
each representing one row in the matrix.

> class ToRowHLists x l | x -> l where toRowHLists :: x -> l
> instance ToRowHLists (Mat HNil a) HNil where toRowHLists _ = HNil
> instance (ToHList (Vec v a) l, ToRowHLists (Mat vs a) ls)
>   => ToRowHLists (Mat (HCons v vs) a) (HCons l ls)
>   where toRowHLists m = HCons (toHList (rowHead m)) (toRowHLists (rowTail m))

This class allows converting an HList of HLists to the equivalent matrix,
where each HList is assumed to represent a row.
TODO: The @HNil@ instance should be removed -- we do not want to allow creation
of empty matrices.

> class FromRowHLists l x | l -> x where fromRowHLists :: l -> x
> instance FromRowHLists HNil (Mat HNil a) where fromRowHLists _ = ListMat []
> instance (FromHList l (Vec v a), FromRowHLists ls (Mat vs a), Cols vs n, HLength v n)
>   => FromRowHLists (HCons l ls) (Mat (HCons v vs) a)
>   where fromRowHLists (HCons l ls) = consRow (fromHList l) (fromRowHLists ls)


Head and tail
-------------
| Return the first row of the matrix.

> rowHead :: Mat (HCons v vs) a -> Vec v a
> rowHead (ListMat vs) = ListVec (head vs)

| Drop the first row of the matrix.
TODO: The @HNil@ instance should be removed -- we do not want to allow creation
of empty matrices.

> rowTail :: Mat (HCons v vs) a -> Mat vs a
> rowTail (ListMat vs) = ListMat (tail vs)


Transpose
=========
Thanks to the @Apply ConsCol@ instances the 'Transpose' instance is pretty simple!
TODO: @transpose mEmpty@ crashes!

> class Transpose vs vs' | vs -> vs' where
>   transpose :: Mat vs a -> Mat vs' a
>   transpose (ListMat []) = ListMat []
>   transpose (ListMat vs) = ListMat (O.transposed vs)
> instance (HHead m v, HMap MkNil v v', HFoldr ConsEach v' m m') => Transpose m m'


Matrix times vector
===================
Multiplying a matrix by a vector. I believe there is some pretty
term that could be used... project??

> class MatrixVector vs v v' | vs v -> v' where
>   matVec :: Num a => Mat vs a -> Vec v a -> Vec v' a
>   matVec (ListMat vs) (ListVec v) = ListVec (O.matrix_ket vs v)

> data MatVec v
> instance DotProduct v1 v2 v3 => Apply (MatVec v2) v1 v3 where apply _ _ = undefined
> instance HMap (MatVec v) m m' => MatrixVector m v m'


Matrix time matrix
==================
Multiplication of two matrices.

> class MatrixMatrix m1 m2 m3 | m1 m2 -> m3 where
>   matMat :: Num a => Mat m1 a -> Mat m2 a -> Mat m3 a
>   matMat (ListMat m) (ListMat m') = ListMat (O.matrix_matrix m (O.transposed m'))

> data MatMat m
> instance MatrixVector m v v' => Apply (MatMat m) v v' where apply _ _ = undefined
> instance (Transpose m2 m2', HMap (MatMat m1) m2' m3) => MatrixMatrix m1 m2 m3


Scale a matrix (multiply by a scalar).

> scaleMat :: (HMap (ScaleV, d) vs1 vs2, Num a) => Quantity d a -> Mat vs1 a -> Mat vs2 a
> scaleMat (Dimensional x) (ListMat vs) = ListMat (fmap (fmap (x P.*)) vs)


Test values

> m1 = rowMatrix v1
> m2 = consRow v2 m1
> m3 = consRow v3 m2
> m4 = consCol v3 m2
> m5 = consRow v4 m1
> m2' = transpose m2
> m3' = transpose m3

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

