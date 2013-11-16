module Tunnel where

{-| Generates a tunnel effect.

@docs tunnel
-}

type State = {time:Float}

make : State
make = {time=0}

step : Float -> State -> State
step delta ({time} as state) = { state | time <- time + delta }

{-| Returns a tunnel effect filled form depending on the current time. -}
display : State -> Form
display {time} = rect 200 200 |> filled (rgb 0 255 255)