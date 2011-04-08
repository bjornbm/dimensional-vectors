import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude


{-
-- | Convert from LVLH frame to ECI frame, assuming zero inclination.
lvlh2eci scra = rotZ scra `matMat` rotX ((-90)*~degree) `matMat` rotY ((-90)*~degree)

inLVLH scra mat = lvlh2eci scra `matMat` mat `matMat` transpose (lvlh2eci scra)

ypys = rotZ (0.081*~degree) `matMat` rotY ((65.125)*~degree) `matMat` rotZ ((-179.833)*~degree)
r = rotX ((1.022)*~degree)

a1 = ypys `matVec` x
a2 = ypys `matMat` r `matVec` x
a3 = ypys `matVec` z

-- Desired final attitude.
ra  = 1.05 *~ degree
dec = 0.01 *~ degree
scra = 270 *~ degree
--att_f = rotY (negate dec) `matMat` rotZ ra `matMat` lvlh2eci scra `matVec` x
att_f = rotZ (negate dec) `matMat` rotY (negate ra) `matVec` x

-- Reverse-engineer the initial vector.
att_i = transpose ypys `matVec` att_f
dec_i = acos (y `dotProduct` att_i) - 90*~degree 
ra_i  = acos (x `dotProduct` att_i)  -- For very small dec'.

-- Compute the problematic final vector and its ra and dec.
att_f' = ypys `matMat` r `matVec` att_i
dec' = acos (y `dotProduct` att_f') - 90*~degree 
ra' = acos (x `dotProduct` att_f')  -- For very small dec'.

-- The angle between the desired final vector and the problematic final vector.
err = acos (att_f `dotProduct` att_f')
-- -}


-- SEQUENCE OF BODY VECTORS, ROW-WISE
-- (we convert the rows to columns in a matrix)
atti = consCol (vCons (( 0.9177274117e+00)*~one) $ vCons ((-0.3972104506e+00)*~one) $ vSing ((-0.5057800831e-03)*~one))
   $   consCol (vCons ((-0.4759639595e-03)*~one) $ vCons (( 0.1736481140e-03)*~one) $ vSing ((-0.9999998717e+00)*~one))
   $ colMatrix (vCons (( 0.3972104874e+00)*~one) $ vCons (( 0.9177275346e+00)*~one) $ vSing ((-0.2969622460e-04)*~one))

-- G-19 AMF1 slew sequence.
rotR = rotX ((   1.022)*~degree) -- ES bias.
rot1 = rotZ ((-179.833)*~degree)
rot2 = rotY (( -65.125)*~degree)
rot3 = rotZ (    0.081 *~degree)

-- Slew without ES bias.
att1 = atti `matMat` rot1
att2 = att1 `matMat` rot2
att3 = att2 `matMat` rot3
z3 = att3 `matVec` z

-- Slew with ES bias.
attiR = atti  `matMat` rotR
att1R = attiR `matMat` rot1
att2R = att1R `matMat` rot2
att3R = att2R `matMat` rot3
z3r = att3R `matVec` z

-- Diff between the two.
err = acos (z3 `dotProduct` z3r)

dec v = 90*~degree - acos (v `dotProduct` z)
ra  v = acos (v `dotProduct` x) -- only for very small declinations
-- 0.9996655998D+00    0.2566988705D-01   -0.3121784926D-02
z_ideal = vCons ((0.9996655998e+00)*~one) $ vCons ((0.2566988705e-01)*~one) $ vSing ((-0.3121784926e-02)*~one)



--8<------- From G19_AMF3.hs ---------8<------------------

-- minus x
negX = rotZ (180*~degree) `matVec` x

sunECI = rotY sunDec `matMat` rotZ sunRA `matVec` x where
  sunRA  = 184.965  *~ degree
  sunDec = (-2.149) *~ degree
sunECI' = rotZ sunRA `matMat` rotY sunDec `matVec` x where
  sunRA  = 184.965  *~ degree
  sunDec = (-2.149) *~ degree

ecassSC = rotY pitchBias `matMat` rotZ yawBias `matVec` negX where
  pitchBias = (-15.038) *~ degree
  yawBias   = ( -0.24 ) *~ degree


