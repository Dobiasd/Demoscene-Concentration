module Tunnel where

{-| Generates a tunnel effect.

@docs tunnel
-}

import Effect(Effect, effect)

-- todo: colored fast starfield in background

type State = {time:Float}

tunnel : State -> Effect
tunnel s = Effect {step = step s, display = display s, name = "Tunnel"}

make : Effect
make = tunnel {time=0}

step : State -> Float -> Effect
step ({time} as state) delta = tunnel { state | time <- time + delta }

{-| Returns a tunnel effect filled form depending on the current time. -}
display : State -> Form
display ({time} as state) =
  group [
    rect 200 200 |> filled (rgb 0 255 255)
  , asText time |> toForm
  ]