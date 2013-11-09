module Plasma where

{-| Generates a plasma effect.

@docs plasma
-}

{-| Returns a plasma effect filled form depending on the current time. -}
plasma : Time -> Form
plasma t = rect 200 200 |> filled (rgb 255 0 0)