module Susurrant.Math.MultivariateGaussian
  ( MultivariateGaussian
  , Sample
  , multivariateGaussian
  , multivariateGaussian'
  , getMean
  , getCovariance
  , gaussExample
  , sample
  , sampleN
  , GAUSS
  ) where

import Prelude
import Control.Monad.Error.Class
import Control.Monad.Eff
import Control.Monad.Except.Trans
import Data.Array
import Data.Either
import Data.Maybe
import Partial.Unsafe (unsafePartial)
import Susurrant.Math.Matrix
import Susurrant.Math.Errors

newtype MultivariateGaussian = MultivariateGaussian
  { mean :: Array Number
  , covariance :: Matrix Number
  }

type Sample = Array Number

multivariateGaussian :: forall m. (MonadError MathError m) => Array Number -> Matrix Number -> m MultivariateGaussian
multivariateGaussian m covariance = do
  let meanSize = length m
      cov = unMatrix covariance
      covN = length cov
  when (covN /= meanSize) (throwError (GaussError CovarianceNotSameSizeAsMean))
  covM <- maybe (throwError (GaussError EmptyCovariance)) pure (map length (head cov))
  when (covM /= covN) (throwError (GaussError NonSquareCovariance))
  -- TODO: check for positive semi-definiteness?
  pure $ MultivariateGaussian { mean: m, covariance: covariance }

multivariateGaussian' :: Array Number -> Matrix Number -> Either MathError MultivariateGaussian
multivariateGaussian' = multivariateGaussian

-- | A simple uncorrelated 2D Gaussian.
gaussExample :: MultivariateGaussian
gaussExample = unsafePartial (fromRight gauss)
  where gauss = do let mean = [0.0, 0.0]
                   cov <- matrix' [[1.0, 0.0], [0.0, 1.0]]
                   multivariateGaussian' mean cov

getMean :: MultivariateGaussian -> Array Number
getMean (MultivariateGaussian {mean, covariance}) = mean

getCovariance :: MultivariateGaussian -> Matrix Number
getCovariance (MultivariateGaussian {mean, covariance}) = covariance

sample :: forall eff. MultivariateGaussian -> Eff (gauss :: GAUSS | eff) Sample
sample = sample_ <<< toSampler_

sampleN :: forall eff. Int -> MultivariateGaussian -> Eff (gauss :: GAUSS | eff) (Array Sample)
sampleN n = sampleN_ n <<< toSampler_

foreign import data GAUSS :: !
foreign import data JSMultivariateNormal :: *

foreign import toSampler_ :: MultivariateGaussian -> JSMultivariateNormal

foreign import sample_ :: forall eff. JSMultivariateNormal -> Eff (gauss :: GAUSS | eff) Sample

foreign import sampleN_ :: forall eff. Int -> JSMultivariateNormal -> Eff (gauss :: GAUSS | eff) (Array Sample)
