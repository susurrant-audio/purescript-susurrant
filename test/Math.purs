module Test.Math where

import Prelude
import Control.Monad.Error.Class
import Data.Array
import Data.Either
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Susurrant.Types
import Susurrant.MultivariateGaussian
import Susurrant.Matrix
import Data.Tuple
import Data.Maybe (fromMaybe)
import Data.Unfoldable (replicate, replicateA)
import Test.QuickCheck ((===), (/==), (<?>))
import Test.Spec (describe, it, pending)
import Test.Spec.QuickCheck (quickCheck)

newtype ArbMatrix = ArbMatrix (Matrix Number)

instance arbMatrix :: Arbitrary ArbMatrix where
  arbitrary = do
    n <- chooseInt 1 32
    m <- chooseInt 1 32
    mat <- replicateA n (vectorOf m uniform)
    either (const arbitrary) (pure <<< ArbMatrix) (matrix' mat)

newtype ArbSquare = ArbSquare (Matrix Number)

instance arbSquare :: Arbitrary ArbSquare where
  arbitrary = do
    n <- chooseInt 1 32
    mat <- replicateA n (vectorOf n uniform)
    case matrix' mat of
      Left e -> arbitrary -- try again
      Right mat' -> pure $ ArbSquare mat'

matrixRowSpec =
  describe "Matrix" $ do
    it "has a number of rows, all the same length" $
      quickCheck (\(ArbMatrix m) -> let mat = unMatrix m
                                        matN = length mat
                                        matM = fromMaybe 0 (map length (head mat))
                                    in map length mat === replicate matN matM)

dimensionsSpec =
  describe "dimensions" $ do
    it "gets the dimensions of a matrix" $ do
      quickCheck $ (\(ArbMatrix mat) -> let dims = dimensions mat
                                        in (dims.rows <= 32 && dims.cols <= 32) === true)

gaussianSpec =
 describe "MultivariateGaussian" $ do
    it "has a mean and a covariance matrix of the same size" $
      quickCheck $ (\(Tuple (ArbSquare cov) mean) -> let dim = dimensions cov
                                                         isSquare = dim.rows == dim.cols
                                                         mg = multivariateGaussian' mean cov
                                                     in isRight mg === (isSquare && length mean == dim.rows))

mathSpec = do
  matrixRowSpec
  dimensionsSpec
  gaussianSpec
