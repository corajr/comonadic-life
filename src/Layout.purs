module App.Layout where

import App.LifeDisplay as LifeDisplay
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, pre, text)
import Data.Show (show)
import Life

data Action
  = Child (LifeDisplay.Action)
  | PageView Route

type State =
  { route :: Route
  , count :: LifeDisplay.State }

init :: State
init =
  { route: NotFound
  , count: LifeDisplay.init }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Child action) state = state { count = LifeDisplay.update action state.count }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> map Child $ LifeDisplay.view state.count
        NotFound -> NotFound.view state
    ]
