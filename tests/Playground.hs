{-
Some vectors and matrices to play with in ghci. If this file copiles
we are already off to a good start.
-}

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Data.HList
import qualified Prelude

-- Test values from old Vector
-- ===========================

l1 = HCons (1.0 *~ meter) $ HCons (2.1 *~ second) $ HNil
l2 = HCons (2.0 *~ hertz) $ HCons ((-0.1) *~ kilo gram) $ HNil
l3 = HCons (133.0 *~ second) $ HCons (2.1 *~ meter) $ HNil
l4 = (2.0 *~ hertz) .*. (22.0 *~ meter ^ neg1) .*. HNil

v1 = fromHList l1
v2 = fromHList l2
v3 = fromHList l3
v4 = fromHList l4
v5 = vCons (3 *~ newton) v4

-- Testing crossProduct with homogeneous and heterogeneous vectors.

vc1 = vCons (3 *~ meter) $ vCons (2 *~ meter) $ vSing (1 *~ meter)
vc2 = vCons (1 *~ hertz) $ vCons (2 *~ hertz) $ vSing (3 *~ hertz)
vc12 = crossProduct vc1 vc2

vc3 = vCons (3.0 *~ meter)            $ vCons (2 *~ one)   $ vSing (1 *~ one)
vc4 = vCons (1   *~ (meter / second)) $ vCons (2 *~ hertz) $ vSing (3 *~ hertz)
vc34 = crossProduct vc3 vc4



-- Test values from old Matrix
-- ===========================

m1 = rowMatrix v1
m2 = consRow v2 m1 
m3 = consRow v3 m2
m4 = consCol v3 m2
m5 = consRow v4 m1
m2' = transpose m2
m3' = transpose m3

m6 = fromRowHLists ((1.1 *~ meter .*. 2 *~ second .*. HNil)
                .*. (3.3 *~ meter .*. 1 *~ second .*. HNil)
                .*. HNil)
m7 = fromRowHLists ((1.1 *~ second .*. 2 *~ meter .*. HNil)
                .*. (3.3 *~ second .*. 1 *~ meter .*. HNil)
                .*. HNil)
m8 = fromRowHLists ((1.1 *~ second .*. 2 *~ second .*. HNil)
                .*. (3.3 *~ meter  .*. 1 *~ meter  .*. HNil)
                .*. HNil)
m6m8 = matMat m6 m8
m8m6 = matMat m8 m6

mm1 = rowMatrix $ fromHList $ _1 .*. 2 *~ second .*. HNil
mm2 = fromRowHLists 
  $   (1 *~ meter ^ pos2            .*. 2 *~ (meter ^ pos2 / second)        .*. HNil)
  .*. (3 *~ (meter ^ pos2 / second) .*. 4 *~ (meter ^ pos2 / second ^ pos2) .*. HNil)
  .*. HNil
vv1 = fromHList (0 *~ meter ^ pos2 .*. 0 *~ (meter ^ pos2 / second) .*. HNil)
