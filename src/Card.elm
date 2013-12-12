module Card where

{-| Representation of a game card.
-}

import Effects.ElmLogo(elmLogo)
import Common.Types(Positioned, Boxed)
import Common.Vector(Point, Positioned, Boxed)
import Effects.Effect(Effect)
import Effects.Effect as Effect

data Status = FaceDown | FaceUp | Done
type Card = Boxed {effect:Effect, status:Status, doneTime:Time}
type Cards = [Card]

{-| Show card according to its state. -}
display : Time -> Card -> Form
display time card =
  let
    texture = case card.status of
                  FaceDown -> backside
                  FaceUp   -> group [ Effect.display card.effect, border ]
                  Done     -> group [ Effect.display card.effect
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
                                else Effect.step effect delta
                  , doneTime <- doneTime' }

{-| Do all cards in the list have the came effect name? -}
allEqual : Cards -> Bool
allEqual cards =
  let
    es = map .effect cards
    equalName (Effect e1) (Effect e2) = e1.name == e2.name
  in
    if length es < 2 then True
      else all (equalName (head es)) (tail es)

{-| The first list of the resulting pair contains all cards with the
given status. The other cards are copied into the second list. -}
splitCardsByStatus : Status -> Cards -> (Cards,Cards)
splitCardsByStatus status = partition (((==) status) . (.status))