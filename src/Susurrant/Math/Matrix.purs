module Susurrant.Math.Matrix
  ( Matrix
  , unMatrix
  , matrix
  , matrix'
  , dimensions
  ) where

import Prelude
import Susurrant.Math.Errors
import Data.Array
import Data.Either
import Data.Foldable (any)
import Control.Monad.Error.Class
import Data.Maybe

newtype Matrix a = Matrix { unMatrix :: Array (Array a) }

unMatrix :: forall a. Matrix a -> Array (Array a)
unMatrix (Matrix m) = m.unMatrix

matrix :: forall a m. (MonadError MathError m) => Array (Array a) -> m (Matrix a)
matrix arr = do
  let lengths = map length arr
      m = fromMaybe 0 (head lengths)
  when (any ((/=) m) lengths) (throwError (MatError RowsNotEqualSize))
  pure (Matrix {unMatrix: arr})

matrix' :: forall a. Array (Array a) -> Either MathError (Matrix a)
matrix' = matrix

dimensions :: forall a. Matrix a -> { rows :: Int, cols :: Int}
dimensions (Matrix {unMatrix: mat }) =
  let n = length mat
      m = fromMaybe 0 (map length (head mat))
  in { rows: n, cols: m }

