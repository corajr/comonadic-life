module App.LifeDisplay where

import Prelude (const)
import Pux.Html (Html, div, pre, button, text)
import Pux.Html.Events (onClick)
import Life

data Action = Evolve

type State = Z Boolean

init :: State
init = glider

update :: Action -> State -> State
update Evolve state = evolve state

view :: State -> Html Action
view state =
  div
    []
    [ pre [] [ text (disp state) ]
    , button [ onClick (const Evolve) ] [ text "Step" ]
    ]
