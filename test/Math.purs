module Test.Math where

import Prelude
import Data.Array
import Data.Maybe (fromMaybe)
import Data.Unfoldable (replicate, replicateA)
import Test.Spec                  (describe, it, pending)
import Test.QuickCheck            ((===), (/==))
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.Spec.QuickCheck       (quickCheck)
import Susurrant.Types

newtype ArbMatrix = ArbMatrix (Matrix Number)

instance arbMatrix :: Arbitrary ArbMatrix where
  arbitrary = do
    n <- chooseInt 1 32
    m <- chooseInt 1 32
    mat <- replicateA n (vectorOf m uniform)
    pure $ ArbMatrix (Matrix {unMatrix: mat})

mathSpec = do
  describe "Matrix" do
    it "has a number of rows, all the same length" $
      quickCheck \(ArbMatrix (Matrix {unMatrix: mat})) ->
        let matN = length mat
            matM = fromMaybe 0 (map length (head mat))
        in map length mat === replicate matN matM
  describe "MultivariateGaussian" do
    pending "has a mean and a covariance matrix"
