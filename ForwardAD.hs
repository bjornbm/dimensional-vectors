{-# OPTIONS_GHC -fglasgow-exts #-}

module ForwardAD (diffV, diffV', liftV, lift)
  where

import Data.HList (HMap)
import Numeric.Units.Dimensional (Dimensional (Dimensional), Quantity, Div, DOne)
import Vector (Vec (ListVec), MulD, DivD)
import Numeric.FAD (Dual, diffUF, diff2UF)
import qualified Numeric.FAD as F (lift)

-- | If @f@ is a function of a quantity that returns a 'Vector', then
-- @diff f@ is a function of the same type of quantity that returns
-- the first derivative of the result.
diffV :: (Num a, HMap (DivD,d) ds ds')
  => (forall tag. Quantity d (Dual tag a) -> Vec ds (Dual tag a)) -> Quantity d a -> Vec ds' a
diffV f = snd . diffV' f
--diffV f (Dimensional x) = ListVec (diffUF (unvec . f . Dimensional) x)

diffV' :: (Num a, HMap (DivD,d) ds ds')  -- Constraint could be changed to infer d instead (or also) if desired.
  => (forall tag. Quantity d (Dual tag a) -> Vec ds (Dual tag a)) -> Quantity d a -> (Vec ds a, Vec ds' a)
diffV' f (Dimensional x) = (ListVec ys, ListVec ys') where (ys,ys') = diff2UF (unvec . f . Dimensional) x


unvec (ListVec xs) = xs


-- | Lift the elements of a vector to 'Fad.Dual's.
liftV :: Num a => Vec ds a -> Vec ds (Dual tag a)
liftV (ListVec xs) = ListVec (map F.lift xs)

lift :: Num a => Dimensional v d a -> Dimensional v d (Dual tag a)
lift (Dimensional x) = Dimensional (F.lift x)

--primalV :: Num a => Vec ds (Dual tag a) -> Vec ds a
--primalV (ListVec xs) = ListVec (fprimal xs)


