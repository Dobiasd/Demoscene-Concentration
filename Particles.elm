module Particles where

{-| Generates a particle effect.

@docs particles
-}

{-| Returns a particle effect filled form depending on the current time. -}
particles : Time -> Form
particles t = rect 200 200 |> filled (rgb 0 255 0)