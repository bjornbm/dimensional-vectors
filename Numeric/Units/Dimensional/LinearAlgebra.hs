module Numeric.Units.Dimensional.LinearAlgebra
  ( module Vector
  , module Matrix
  , module Numeric.Units.Dimensional.LinearAlgebra.Operators
  , (:*.)
  ) where

import Vector hiding (vMap, vZipWith)
import Matrix
import Numeric.Units.Dimensional.LinearAlgebra.Operators
import Numeric.Units.Dimensional.LinearAlgebra.HListExtras ((:*.))
