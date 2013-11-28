module Tunnel where

{-| Generates a tunnel effect.

@docs tunnel
-}

-- todo: colored fast starfield in background

import Effect(Effect, effect)
import Effect
import Starfield

type State = {time:Float, background:Effect}

tunnel : State -> Effect
tunnel s = Effect {step = step s, display = display s, name = "Tunnel"}

make : Effect
make = tunnel {time=0, background=Starfield.make Starfield.Colored 4 128}

step : State -> Float -> Effect
step ({time,background} as state) delta =
  tunnel { state | time <- time + delta
                    , background <- Effect.step background delta }

{-| Returns a tunnel effect filled form depending on the current time. -}
display : State -> Form
display ({time,background} as state) =
  Effect.display background
  --group [ rect 200 200 |> filled (rgb 0 255 255) ]