module App.Layout where

import App.Counter as Counter
import Susurrant.Math as Math
import Susurrant.Components.ShowGauss as ShowGauss
import Susurrant.Components.Effects (AppEffects)
import Pux (noEffects, mapEffects, mapState, EffModel)
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude (($), map, pure)
import Pux.Html (Html, div, h1, p, text)

data Action
  = GaussView (ShowGauss.Action)
  | PageView Route

type State =
  { route :: Route
  , gauss :: ShowGauss.State }

init :: State
init =
  { route: NotFound
  , gauss: ShowGauss.init Math.gaussExample }


update :: Action -> State -> EffModel State Action AppEffects
update (PageView Home) state =
  { state: state { route = Home }
  , effects: [ pure (GaussView ShowGauss.Resample) ]
  }
update (PageView route) state = noEffects $ state { route = route }
update (GaussView action) state =
  mapEffects GaussView (mapState (state { gauss = _ }) $ ShowGauss.update action state.gauss)

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> map GaussView $ ShowGauss.view state.gauss
        NotFound -> NotFound.view state
    ]
