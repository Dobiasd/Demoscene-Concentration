module Card where

import ElmLogo(elmLogo)
import Common(Point, Positioned, Boxed)
import Effect(Effect)
import Effect

data Status = FaceDown | FaceUp | Done

type Card = Boxed {effect:Effect, status:Status}

display : Time -> Card -> Form
display time card =
  let
    texture = case card.status of
                  FaceDown -> backside
                  --FaceDown -> card.effect.display
                  FaceUp -> group [ Effect.display card.effect, border ]
                  --Done -> group [f card.effect, doneOverlay time]
                  Done -> rect 0 0 |> filled (rgb 0 0 0)
  in
    texture |> move (card.x, card.y) |> scale (card.w / 200)

border : Form
border =
  let
    width = 10
    lsGray = solid gray
    grayLSWide = { lsGray | width <- width, join <- Smooth, cap <- Round }
  in
    rect (206 - width) (206 - width) |> outlined grayLSWide

backside : Form
backside = group [ border, elmLogo ]

doneOverlay : Time -> Form
doneOverlay _ = rect 200 200 |> filled (rgba 0 0 0 0.5)

make effect box =
  let
    boxedEffect = { box | effect=effect }
  in
    { boxedEffect | status=FaceDown }

step : Float -> Card -> Card
step delta ({status, effect} as card) =
  case status of
    FaceUp -> { card | effect <- Effect.step effect delta }
    _ -> card