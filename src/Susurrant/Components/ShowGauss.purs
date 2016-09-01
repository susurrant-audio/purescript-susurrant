module Susurrant.Components.ShowGauss where

import Prelude
import Data.Maybe
import Susurrant.Math
import Susurrant.Components.Effects (AppEffects)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Class (liftEff)
import Pux (noEffects, EffModel)
import Pux.Html (Html, Attribute, div, span, dl, dt, dd, ul, li, button, text)

type State =
  { gaussian :: MultivariateGaussian
  , nSamples :: Int
  , samples :: Maybe (Array Sample)}

init :: MultivariateGaussian -> State
init gauss =
  { gaussian: gauss
  , nSamples: 10
  , samples: Nothing
  }

data Action = Resample
            | SetSamples (Array Sample)

update :: Action -> State -> EffModel State Action AppEffects
update Resample state =
  { state: state
  , effects: [ do
      samples <- liftEff (sampleN state.nSamples state.gaussian)
      pure $ SetSamples samples
    ]
  }
update (SetSamples samples) state =
  noEffects (state { samples = Just samples})

viewSample :: Sample -> Html Action
viewSample sample = div [] [ text (show sample) ]

view :: State -> Html Action
view state =
  case state.samples of
    Just samples' -> div [] (map viewSample samples')
    Nothing -> div [] [ text "No samples yet"]
