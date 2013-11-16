module Lissajous where

{-| Generates a lissajous effect.

@docs lissajous
-}

type State = {time:Float}

make : State
make = {time=0}

step : Float -> State -> State
step delta ({time} as state) = { state | time <- time + delta }

{-| Returns a lissajous effect filled form depending on the current time. -}
display : State -> Form
display {time} = rect 200 200 |> filled (rgb 255 0 255)