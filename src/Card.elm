module Card where

import ElmLogo(elmLogo)
import Common(Point, Positioned, Boxed)
import Effect(Effect)
import Effect


data Status = FaceDown | FaceUp | Done

type Card = Boxed {effect:Effect, status:Status, doneTime:Time}

display : Time -> Card -> Form
display time card =
  let
    texture = case card.status of
                  FaceDown -> backside
                  FaceUp -> group [ Effect.display card.effect, border ]
                  Done -> group [ Effect.display card.effect, border, doneOverlay ]
  in
    texture |> move (card.x, card.y) |> scale (card.w / 200)

isGone : Card -> Bool
isGone {doneTime} = doneTime > 1000

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

doneOverlay : Form
doneOverlay = rect 200 200 |> filled (rgba 0 0 0 0.4)

make effect box =
  let
    boxedEffect = { box | effect=effect }
    boxedEffectWithStatus = { boxedEffect | status=FaceDown }
  in
    { boxedEffectWithStatus | doneTime=0 }


step : Float -> Card -> Card
step delta ({status, effect, doneTime} as card) =
  let
    doneTime' = if status == Done then doneTime + delta else doneTime
  in
    case status of
      FaceDown -> card
      _ -> { card | effect <- Effect.step effect delta
                  , doneTime <- doneTime' }