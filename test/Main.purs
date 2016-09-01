module Test.Main where

import Prelude
import Test.Math (mathSpec)
import Test.ShowGauss (showGaussSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main = run [consoleReporter] do
  mathSpec
  showGaussSpec
