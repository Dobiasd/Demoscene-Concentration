module Card where

{-| Representation of a game card.
-}

import Color exposing (gray, rgba)
import Graphics.Collage exposing (group, move, scale, Form, solid
  , LineJoin(Smooth), LineCap(Round), rect, outlined, filled)
import List exposing (map, length, all, head, tail, partition)
import Time exposing (Time)

import Common.Algorithms exposing (unsafeTail, unsafeHead
  )
import Effects.ElmLogo exposing (elmLogo)
import Common.Types exposing (Positioned, Boxed)
import Effects.Effect as Eff

type Status = FaceDown | FaceUp | Done
type alias Card = Boxed {effect:Eff.Effect, status:Status, doneTime:Time}
type alias Cards = List Card

{-| Show card according to its state. -}
display : Time -> Card -> Form
display time card =
  let
    texture = case card.status of
                  FaceDown -> backside
                  FaceUp   -> group [ Eff.display card.effect, border ]
                  Done     -> group [ Eff.display card.effect
                                    , border
                                    , doneOverlay ]
  in
    texture |> move (card.x, card.y) |> scale (card.w / 200)


{-| After how many milliseconds should done card disappear completely? -}
goneAfterTime = 1000

{-| Check if a card is gone. -}
isGone : Card -> Bool
isGone {doneTime} = doneTime > goneAfterTime

{-| Draw the border for a card. -}
border : Form
border =
  let
    width = 10
    lsGray = solid gray
    grayLSWide = { lsGray | width <- width, join <- Smooth, cap <- Round }
  in
    rect (206 - width) (206 - width) |> outlined grayLSWide

{-| Backside of a card. -}
backside : Form
backside = group [ border, elmLogo ]

{-| Make done cards darker. -}
doneOverlay : Form
doneOverlay = rect 200 200 |> filled (rgba 0 0 0 0.4)

{-| Create a card with an effect and a gived box (position and size). -}
make effect box =
  let
    boxedEffect = { box | effect=effect }
    boxedEffectWithStatus = { boxedEffect | status=FaceDown }
  in
    { boxedEffectWithStatus | doneTime=0 }

{-| Step a cards state by a time delta. -}
step : Float -> Card -> Card
step delta ({status, effect, doneTime} as card) =
  let
    -- Track how long the card is done to set in to gone at the right moment.
    doneTime' = if status == Done then doneTime + delta else doneTime
  in
    case status of
      FaceDown -> card
      _ -> { card | effect <- if isGone card
                                then effect
                                else Eff.step effect delta
                  , doneTime <- doneTime' }

{-| Do all cards in the list have the came effect name? -}
allEqual : Cards -> Bool
allEqual cards =
  let
    es = map .effect cards
    equalName (Eff.Effect e1)
              (Eff.Effect e2) = e1.name == e2.name
  in
    if length es < 2 then True
      else all (equalName (unsafeHead es)) (unsafeTail es)

{-| The first list of the resulting pair contains all cards with the
given status. The other cards are copied into the second list. -}
splitCardsByStatus : Status -> Cards -> (Cards,Cards)
splitCardsByStatus status = partition (.status >> (==) status)