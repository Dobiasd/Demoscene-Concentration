module Card where

import ElmLogo(elmLogo)
import Common(Point, Positioned, Boxed)
import Effect(Effect)

data Status = FaceDown | FaceUp | Done

type Card = Boxed {effect:Effect, status:Status}

border : Form
border =
  let
    width = 10
    lsGray = solid gray
    grayLSWide = { lsGray | width <- width, join <- Smooth, cap <- Round }
    lsWhite = solid white
    whiteLSWide = { lsWhite | width <- width, cap <- Padded }
  in
    group [
      rect (202 - width) (202 - width) |> outlined whiteLSWide
    , rect (202 - width) (202 - width) |> outlined grayLSWide
    ]

backside : Time -> Form
backside _ = group[ border, elmLogo ]


doneOverlay : Time -> Form
doneOverlay _ = rect 200 200 |> filled (rgba 0 0 0 0.5)


make effect box =
  let boxedEffect = { box | effect=effect }
  in { boxedEffect | status=FaceDown }


step : Float -> Card -> Card
step delta ({status, effect} as card) =
  let
    f (Effect ef) = ef.step
  in
    case status of
      FaceUp -> { card | effect <- f effect delta }
      _ -> card