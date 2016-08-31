module Susurrant.MultivariateGaussian
  ( MultivariateGaussian
  , multivariateGaussian
  , multivariateGaussian'
  , getMean
  , getCovariance
  , sample
  , GAUSS
  ) where

import Prelude
import Control.Monad.Error.Class
import Control.Monad.Eff
import Control.Monad.Except.Trans
import Data.Array
import Data.Either
import Data.Maybe
import Susurrant.Matrix
import Susurrant.Types

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

getMean :: MultivariateGaussian -> Array Number
getMean (MultivariateGaussian {mean, covariance}) = mean

getCovariance :: MultivariateGaussian -> Matrix Number
getCovariance (MultivariateGaussian {mean, covariance}) = covariance

sample :: forall eff. MultivariateGaussian -> Eff (gauss :: GAUSS | eff) (Array Number)
sample = sample_ <<< toSampler_

foreign import data GAUSS :: !
foreign import data JSMultivariateNormal :: *

foreign import toSampler_ :: MultivariateGaussian -> JSMultivariateNormal

foreign import sample_ :: forall eff. JSMultivariateNormal -> Eff (gauss :: GAUSS | eff) (Array Number)
