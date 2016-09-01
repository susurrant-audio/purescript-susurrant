module Test.Math where

import Prelude
import Data.Traversable (for)
import Control.Monad.Eff.Class
import Data.Array
import Data.Either
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Susurrant.Math
import Data.Tuple
import Data.Maybe (fromMaybe)
import Data.Unfoldable (class Unfoldable, replicate)
import Data.List.Lazy (replicateM)
import Test.QuickCheck ((===), (/==), (<?>))
import Test.QuickCheck.Gen (chooseInt, vectorOf, uniform)
import Test.Spec (describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

newtype ArbMatrix = ArbMatrix (Matrix Number)

instance arbMatrix :: Arbitrary ArbMatrix where
  arbitrary = do
    n <- chooseInt 1 32
    m <- chooseInt 1 32
    mat <- vectorOf n (vectorOf m uniform)
    either (const arbitrary) (pure <<< ArbMatrix) (matrix' mat)

newtype ArbSquare = ArbSquare (Matrix Number)

instance arbSquare :: Arbitrary ArbSquare where
  arbitrary = do
    n <- chooseInt 1 32
    mat <- vectorOf n (vectorOf n uniform)
    either (const arbitrary) (pure <<< ArbSquare) (matrix' mat)

newtype ArbNormal = ArbNormal MultivariateGaussian

instance arbNormal :: Arbitrary ArbNormal where
  arbitrary = do
    (ArbSquare mat) <- arbitrary
    let dims = dimensions mat
    mean <- vectorOf dims.rows uniform
    either (const arbitrary) (pure <<< ArbNormal) (multivariateGaussian' mean mat)

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
    it "can be sampled" do
      x <- liftEff $ sample gaussExample
      length x `shouldEqual` length (getMean gaussExample)
    it "can be sampled a large number of times" do
      xs <- liftEff $ (sampleN 100 gaussExample)
      for xs $ \x ->
        length x `shouldEqual` length (getMean gaussExample)
      pure unit


mathSpec = do
  matrixRowSpec
  dimensionsSpec
  gaussianSpec
