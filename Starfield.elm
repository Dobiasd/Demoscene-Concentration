module Starfield where

{-| Generates a starfield effect.

@docs starfield
-}

type State = {time:Float}

make : State
make = {time=0}

step : Float -> State -> State
step delta ({time} as state) = { state | time <- time + delta }

{-| Returns a starfield effect filled form depending on the current time. -}
display : State -> Form
display {time} = rect 200 200 |> filled (rgb 0 0 255)