{-# OPTIONS_GHC -fglasgow-exts #-}

module ForwardAD where

import qualified Prelude
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional (Dimensional (Dimensional))
import Data.HList (HMap)
import MyHList (HZipWith)
import Numeric.Units.Dimensional (Dimensional (Dimensional), Quantity, Div, DOne)
import Vector (Vec (ListVec), MulD, DivD, Homo, elemAdd, scaleVec)
import Numeric.AD (AD, diffF, diffF', Mode)
import qualified Numeric.AD as F (lift)


-- | If @f@ is a function of a quantity that returns a 'Vector', then
-- @diff f@ is a function of the same type of quantity that returns
-- the first derivative of the result.
diffV :: (Num a, HMap (DivD,d) ds ds')
  => (forall tag. Mode tag => Quantity d (AD tag a) -> Vec ds (AD tag a)) -> Quantity d a -> Vec ds' a
diffV f = snd . diffV' f


unfzip as = (fmap fst as, fmap snd as)

-- | Like 'diffV' but returns a pair of the result and its first derivative.
diffV' :: (Num a, HMap (DivD,d) ds ds')  -- Constraint could be changed to infer d instead (or also) if desired.
  => (forall tag. Mode tag => Quantity d (AD tag a) -> Vec ds (AD tag a))
  -> Quantity d a -> (Vec ds a, Vec ds' a)
diffV' f (Dimensional x) = (ListVec ys, ListVec ys')
  where
    (ys,ys') = unfzip $ diffF' (unvec . f . Dimensional) x
    unvec (ListVec xs) = xs


-- Linearizing
-- -----------

-- @applyLinear@ converts a pair of a vector and its derivative w r t a
-- variable (e g time) into a function  linearized about the original vector
-- at @t=0@. Then the function (which should be independent of the variable,
-- but see 'applyLinearAt') is evaluated and the "new" vector/derivative
-- pair is reconstructed from the result.
applyLinear :: forall a t ds ds' ds2 ds2' ts. (
               Real a, Fractional a,
               HMap (MulD,t) ds' ds,               -- Used in linearization.
               HMap (DivD,t) ds2 ds2',             -- Used in differentiation.
               HZipWith DivD ds ds' ts, Homo ts t  -- Necessary to infer t (the dimension w r t which we are differentiating).
          ) => (forall tag. Mode tag => Vec ds (AD tag a) -> Vec ds2 (AD tag a)) -> (Vec ds a, Vec ds' a) -> (Vec ds2 a, Vec ds2' a)
applyLinear f (p,v) = diffV' (\t -> f (liftV p `elemAdd` scaleVec t (liftV v))) t_0
  where
    t_0  = Dimensional 0 :: Quantity t a

-- 'applyLinearAt is analogous to 'applyLinear' but should be used when
-- the function is also dependent on the variable w r t which the vector
-- is linearized.
applyLinearAt :: forall a t ds ds' ds2 ds2' ts. (
               Real a, Fractional a,
               HMap (MulD,t) ds' ds,               -- Used in linearization.
               HMap (DivD,t) ds2 ds2',             -- Used in differentiation.
               HZipWith DivD ds ds' ts, Homo ts t  -- Necessary to infer t (the dimension w r t which we are differentiating).
          ) => (forall tag. Mode tag => Quantity t (AD tag a) -> Vec ds (AD tag a) -> Vec ds2 (AD tag a))
            -> Quantity t a -> (Vec ds a, Vec ds' a) -> (Vec ds2 a, Vec ds2' a)
applyLinearAt f t (p,v) = diffV' (\t' -> f t' (liftV p `elemAdd` scaleVec (t' - lift t) (liftV v))) t


-- Lifting
-- -------

-- | Lift the elements of a vector to 'AD.AD's.
liftV :: (Mode tag, Num a) => Vec ds a -> Vec ds (AD tag a)
liftV (ListVec xs) = ListVec (map F.lift xs)

-- | Lift a Dimensional.
lift :: (Mode tag, Num a) => Dimensional v d a -> Dimensional v d (AD tag a)
lift (Dimensional x) = Dimensional (F.lift x)

--primalV :: Num a => Vec ds (AD tag a) -> Vec ds a
--primalV (ListVec xs) = ListVec (fprimal xs)
