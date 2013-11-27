module Moire where

{-| Generates a moire effect.

@docs moire
-}

import Effect(Effect, effect)

-- todo: colored fast starfield in background

type State = {time:Float}

moire : State -> Effect
moire s = Effect {step = step s, display = display s, name = "Moire"}

make : Effect
make = moire {time=0}

step : State -> Float -> Effect
step ({time} as state) delta = moire { state | time <- time + delta }

{-| Returns a moire effect filled form depending on the current time. -}
display : State -> Form
display ({time} as state) =
  group [
    rect 200 200 |> filled (rgb 0 127 255)
  , asText time |> toForm
  ]