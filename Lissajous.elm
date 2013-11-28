module Lissajous where

{-| Generates a lissajous effect.

@docs lissajous
-}

-- todo: like vector ball in second reality,
--       very slow and dark starfield in background
--       shadows.

import Effect(Effect, effect)
import Effect
import Starfield

type State = {time:Float, background:Effect}

lissajous : State -> Effect
lissajous s = Effect {step = step s, display = display s, name = "Lissajous"}

make : Effect
make = lissajous {time=0, background=Starfield.make Starfield.BW 0.3 64}

step : State -> Float -> Effect
step ({time,background} as state) delta =
  lissajous { state | time <- time + delta
                    , background <- Effect.step background delta }

{-| Returns a lissajous effect filled form depending on the current time. -}
display : State -> Form
display ({time,background} as state) =
  Effect.display background
  --group [ rect 200 200 |> filled (rgb 255 0 255) ]