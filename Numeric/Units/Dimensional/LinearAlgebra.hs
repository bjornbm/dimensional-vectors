module Numeric.Units.Dimensional.LinearAlgebra
  ( module Numeric.Units.Dimensional.LinearAlgebra.Vector
  , module Numeric.Units.Dimensional.LinearAlgebra.Matrix
  , module Numeric.Units.Dimensional.LinearAlgebra.Operators
  , (:*:), (:*.)
  ) where

import Numeric.Units.Dimensional.LinearAlgebra.Vector hiding (ListVec, vMap, vZipWith)
import Numeric.Units.Dimensional.LinearAlgebra.Matrix hiding (ListMat)
import Numeric.Units.Dimensional.LinearAlgebra.Operators
import Numeric.Units.Dimensional.LinearAlgebra.HListExtras ((:*.))
import Data.HList ((:*:))

{-
QUESTIONS:
  - Should classes have functions or not?
  - Should I define classes for each function? Would it make contraints
more clear or less clear? Would it hide HList type classes?
  - Should I prevent empty vectors/matrices from ever being constructed? Could they cause issues when I change underlying implementation?
  - Want to change to an efficient underlying implementation, but which one? Would prefer a pure Haskell one with minimal dependencies. Would it be possible to offer several by parameterization (the a type variable)?

TODO:
  - Convert from .lhs / write proper haddocks.
-}
