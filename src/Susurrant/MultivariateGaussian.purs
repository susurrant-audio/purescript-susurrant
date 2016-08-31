module Susurrant.MultivariateGaussian
  ( MultivariateGaussian
  , multivariateGaussian
  , multivariateGaussian'
  ) where

import Prelude
import Control.Monad.Error.Class
import Control.Monad.Except.Trans
import Data.Array
import Data.Either
import Data.Maybe
import Susurrant.Matrix
import Susurrant.Types
import Control.Monad.Trans (lift)

newtype MultivariateGaussian = MultivariateGaussian
  { mean :: Array Number
  , covariance :: Matrix Number
  }

multivariateGaussian :: forall m. (MonadError MathError m) => Array Number -> Matrix Number -> m MultivariateGaussian
multivariateGaussian m covariance = do
  let meanSize = length m
      cov = unMatrix covariance
      covN = length cov
  when (covN /= meanSize) (throwError (GaussError CovarianceNotSameSizeAsMean))
  covM <- maybe (throwError (GaussError EmptyCovariance)) pure (map length (head cov))
  when (covM /= covN) (throwError (GaussError NonSquareCovariance))
  pure $ MultivariateGaussian { mean: m, covariance: covariance }

multivariateGaussian' :: Array Number -> Matrix Number -> Either MathError MultivariateGaussian
multivariateGaussian' = multivariateGaussian
