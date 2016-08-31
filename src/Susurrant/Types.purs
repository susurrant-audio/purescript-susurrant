module Susurrant.Types where

import Prelude

newtype Matrix a = Matrix { unMatrix :: Array (Array a) }
