module Sinescroller where

{-| Generates a sine scroller effect.

@docs sinescroller
-}

import Effect(Effect, effect)

type State = {time:Float}

sinescroller : State -> Effect
sinescroller s = Effect {step=step s, display=display s, name="Sinescroller"}

make : Effect
make = sinescroller {time=0}

step : State -> Float -> Effect
step ({time} as state) delta = sinescroller { state | time <- time + delta }

{-| Returns a sine scroller effect filled form
depending on the current time. -}
display : State -> Form
display ({time} as state) =
    group [
      rect 200 200 |> filled (rgb 255 255 0)
    , asText time |> toForm
    ]