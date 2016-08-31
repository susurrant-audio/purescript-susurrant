module Test.Main where

import Prelude
import Test.Math (mathSpec)
import Test.Spec.Runner           (run)
import Test.Spec.Reporter.Console (consoleReporter)

main = run [consoleReporter] do
  mathSpec
