module Numeric.Units.Dimensional.LinearAlgebra
  ( module Numeric.Units.Dimensional.LinearAlgebra.Vector
  , module Numeric.Units.Dimensional.LinearAlgebra.Matrix
  , module Numeric.Units.Dimensional.LinearAlgebra.Operators
  , (:*.)
  ) where

import Numeric.Units.Dimensional.LinearAlgebra.Vector hiding (vMap, vZipWith)
import Numeric.Units.Dimensional.LinearAlgebra.Matrix
import Numeric.Units.Dimensional.LinearAlgebra.Operators
import Numeric.Units.Dimensional.LinearAlgebra.HListExtras ((:*.))
