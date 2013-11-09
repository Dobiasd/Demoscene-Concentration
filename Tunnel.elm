module Tunnel where

{-| Generates a tunnel effect.

@docs tunnel
-}

{-| Returns a tunnel effect filled form depending on the current time. -}
tunnel : Time -> Form
tunnel t = rect 200 200 |> filled (rgb 0 255 255)