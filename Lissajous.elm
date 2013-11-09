module Lissajous where

{-| Generates a lissajous effect.

@docs lissajous
-}

{-| Returns a lissajous effect filled form depending on the current time. -}
lissajous : Time -> Form
lissajous t = rect 200 200 |> filled (rgb 255 0 255)