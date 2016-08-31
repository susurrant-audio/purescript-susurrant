module Susurrant.MultivariateGaussian
  ( MultivariateGaussian
  ) where

import Prelude
-- import Control.Applicative
import Control.Monad.Error.Class
import Data.Array
import Data.Maybe
import Susurrant.Types

newtype MultivariateGaussian = MultivariateGaussian
  { mean :: Array Number
  , covariance :: Matrix Number
  }

data GaussianError = NonSquareCovariance
                   | EmptyCovariance
                   | CovarianceNotSameSizeAsMean

multivariateGaussian :: forall m. (MonadError GaussianError m) => Array Number -> Matrix Number -> m MultivariateGaussian
multivariateGaussian m (Matrix {unMatrix: cov}) = do
  let meanSize = length m
      covN = length cov
  when (covN /= meanSize) (throwError CovarianceNotSameSizeAsMean)
  covM <- maybe (throwError EmptyCovariance) pure (map length (head cov))
  when (covM /= covN) (throwError NonSquareCovariance)
  pure $ MultivariateGaussian { mean: m, covariance: Matrix {unMatrix: cov} }
