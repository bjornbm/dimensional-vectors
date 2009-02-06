import Numeric.Units.Dimensional.Prelude
import Vector
import PosVel
import qualified Prelude


posvel = ( fromTuple (    4383.9449203752        *~ kilo meter :: Length Double
                     , (-41940.917505092)        *~ kilo meter :: Length Double
                     ,      22.790255916589      *~ kilo meter :: Length Double
                     ) 
         , fromTuple (       3.0575666627812     *~ (kilo meter / second) :: Velocity Double
                     ,       0.32047068607303    *~ (kilo meter / second) :: Velocity Double
                     ,       0.00084729371755294 *~ (kilo meter / second) :: Velocity Double
                     )
         )

semiMajorAxis :: RealFloat a => GravitationalParameter a -> CPosVel a -> Length a
semiMajorAxis mu (pos, vel) = negate mu / _2 / (e_pot + e_kin)
  where
    r = vNorm pos
    v = vNorm vel
    e_pot = negate mu / r
    e_kin = v^pos2 / _2

mu_FDS = 398600.44 *~ (kilo meter^pos3 / second^pos2)
mu_STA = 398601.19 *~ (kilo meter^pos3 / second^pos2)


{-
-- | Approximation of geostationary semi-major axis.
semiMajorAxisGEO mu =  cbrt (mu / phi^pos2)
  where
    phi = _2 * pi / (1*~day) -- Approximation of Earth's rotation rate.
-}

printAs :: Fractional a => String -> Quantity d a -> Unit d a -> IO ()
printAs desc val unit = putStrLn $ desc ++ ": " ++ show (val/~unit)

sma_FDS = semiMajorAxis mu_FDS posvel
sma_STA = semiMajorAxis mu_STA posvel

main = do
  --print "GEO Semi-major axis in FDS [km]:"
  --print $ semiMajorAxisGEO mu_FDS /~ kilo meter
  --print "GEO Semi-major axis in STA [km]:"
  --print $ semiMajorAxisGEO mu_STA /~ kilo meter
  printAs "Semi-major axis in FDS [km]" sma_FDS (kilo meter)
  printAs "Semi-major axis in STA [km]" sma_STA (kilo meter)
  let dsma = sma_FDS - sma_STA
  printAs "Difference [km]" dsma (kilo meter)
  printAs "Drift rate difference [deg / day]" ((-0.0128)*~(degree / day / kilo meter) * dsma) (degree / day)

