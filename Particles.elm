module Particles where

{-| Generates a particle effect.

@docs particles
-}

import Effect(Effect, effect)

type State = {time:Float}

particles : State -> Effect
particles s = Effect {step = step s, display = display s, name = "Particles"}

make : Effect
make = particles {time=0}

step : State -> Float -> Effect
step ({time} as state) delta = particles { state | time <- time + delta }

{-| Returns a particle effect filled form depending on the current time. -}
display : State -> Form
display ({time} as state) =
  group [
    rect 200 200 |> filled (rgb 0 255 0)
  , asText time |> toForm
  ]