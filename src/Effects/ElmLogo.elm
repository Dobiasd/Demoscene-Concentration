module Effects.ElmLogo where

{-| Generates a the Elm logo.
-}

import Color exposing (rgb)
import Graphics.Collage exposing (Form, move, polygon, filled, groupTransform
  , scale)

import Transform2D

{-| Returns the Elm logo as a centered 200x200 Form. -}
elmLogo : Form
elmLogo =
  let
    p1 = polygon [(266, 32), (156, 32), (266, 142)]
           |> filled (rgb 96 181 204) |> move (-150, -150)
    p2 = polygon [(148, 32), (36, 32), (89, 85), (201, 85)]
           |> filled (rgb 141 215 55) |> move (-150, -150)
    p3 = polygon [(33, 37), (33, 260), (144, 149)]
           |> filled (rgb 90 99 120) |> move (-150, -150)
    p4 = polygon [(149, 153), (37, 265), (261, 265)]
           |> filled (rgb 96 181 204) |> move (-150, -150)
    p5 = polygon [(267, 152), (210.5, 208.5), (267, 265)]
           |> filled (rgb 239 165 0) |> move (-150, -150)
    p6 = polygon [(206, 89), (92, 89), (148, 146)]
           |> filled (rgb 239 165 0) |> move (-150, -150)
    p7 = polygon [(209.5, 92.5), (152.5, 148.5), (208, 204), (265, 148)]
           |> filled (rgb 138 212 55) |> move (-150, -150)
    mat = Transform2D.matrix 1 0 0 -1 0 0
  in
    groupTransform mat [ p1, p2, p3, p4, p5, p6, p7 ] |> scale (2.0/3.0)