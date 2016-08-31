module Susurrant.Types
  ( MatrixError(..)
  , GaussianError(..)
  , MathError(..)
  ) where

data MatrixError = RowsNotEqualSize

data GaussianError = NonSquareCovariance
                   | EmptyCovariance
                   | CovarianceNotSameSizeAsMean

data MathError = GaussError GaussianError
               | MatError MatrixError
