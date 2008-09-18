import PosVel
import Vector
import Matrix
import Numeric.Units.Dimensional.Prelude
import qualified Prelude


-- | Convert from LVLH frame to ECI frame, assuming zero inclination.
lvlh2eci scra = rotZ scra `matMat` rotX ((-90)*~degree) `matMat` rotY ((-90)*~degree)

inLVLH scra mat = lvlh2eci scra `matMat` mat `matMat` transpose (lvlh2eci scra)

ypys = rotZ (0.081*~degree) `matMat` rotY ((65.125)*~degree) `matMat` rotZ ((-179.833)*~degree)
r = rotX ((1.022)*~degree)

x = vCons _1 $ vCons _0 $ vSing _0
y = vCons _0 $ vCons _1 $ vSing _0
z = vCons _0 $ vCons _0 $ vSing _1
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
