module Starfield where

{-| Generates a starfield effect.

@docs starfield
-}

import Effect(Effect, effect)

type State = {time:Float}

starfield : State -> Effect
starfield s = Effect {step = step s, display = display s, name = "Starfield"}

make : Effect
make = starfield {time=0}

step : State -> Float -> Effect
step ({time} as state) delta = starfield { state | time <- time + delta }

{-| Returns a starfield effect filled form depending on the current time. -}
display : State -> Form
display ({time} as state) =
  group [
    rect 200 200 |> filled (rgb 0 0 255)
  , asText time |> toForm
  ]