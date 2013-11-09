module Sinescroller where

{-| Generates a sine scroller effect.

@docs sinescroller
-}

{-| Returns a sine scroller effect filled form
depending on the current time. -}
sinescroller : Time -> Form
sinescroller t = rect 200 200 |> filled (rgb 255 255 0)