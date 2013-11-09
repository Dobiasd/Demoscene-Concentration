module Starfield where

{-| Generates a starfield effect.

@docs starfield
-}

{-| Returns a starfield effect filled form depending on the current time. -}
starfield : Time -> Form
starfield t = rect 200 200 |> filled (rgb 0 0 255)