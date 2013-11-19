module Lissajous where

{-| Generates a lissajous effect.

@docs lissajous
-}

import Effect(Effect, effect)

type State = {time:Float}

lissajous : State -> Effect
lissajous s = Effect {step = step s, display = display s, name = "Lissajous"}

make : Effect
make = lissajous {time=0}

step : State -> Float -> Effect
step ({time} as state) delta = lissajous { state | time <- time + delta }

{-| Returns a lissajous effect filled form depending on the current time. -}
display : State -> Form
display ({time} as state) =
    group [
      rect 200 200 |> filled (rgb 255 0 255)
    ]