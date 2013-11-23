module DemosceneConcentration where

{-| The classical memory game with old school demoscene effects.

The game begins when the player turns over
the first card. Maximally two cards at a time can be turned over.
If a pair it found, it stays image up. After all pairs are found the
needed time inversly indicates the players performance. ;-)
-}

import Touch
import Window

import Effect(Effect,equalType)

import Common(Point, Positioned, Boxed, Box, point, box)

import Card
import Card(Card)
import Lissajous
import Plasma
import Particles
import Sinescroller
import Starfield
import Tunnel

-- todo: still show cards a second or so after a pair is found
-- todo: move all into src directory. move effects into effects directory.
-- todo: draw Elm logo onto card backsides

-- /---------------------\
-- | model configuration |
-- \---------------------/

{-| The game field extends from -100 to +100 in x and y coordinates. -}
(gameWidth,gameHeight) = (200,200)
framesPerSecond = 60


-- /--------------------\
-- | view configuration |
-- \--------------------/

timeTextHeight = 5
timeTextPosY = 100


-- /--------\
-- | inputs |
-- \--------/

data Action = Tap Point (Int,Int) | Step Float
type Input = { action:Action }

speed : Signal Time
speed = fps framesPerSecond

input : Signal Input
input = (Input <~ actions)

steps : Signal Action
steps = lift Step speed

flips : Signal Action
flips =
  let
    f t winDims = Tap (point (toFloat t.x) (toFloat t.y)) winDims
  in
    f <~ Touch.taps ~ Window.dimensions |> sampleOn Touch.taps |> dropRepeats

actions = merge steps flips


-- /-------\
-- | model |
-- \-------/


type Cards = [Card.Card]


{-| Creation of one single row of cards with equidistant gaps. -}
cardBoxRow : Float -> [Box]
cardBoxRow y =
  let
    cols = 4
    distX = 60
    xOff = -distX * (toFloat cols - 1) / 2
    cardWidth = 55
    cardHeight = 55
  in
    map (\x -> box (distX * x + xOff) y cardWidth cardHeight) [0..cols-1]

cardBoxes =
  let
    rows = 3
    distY = 65
    yOff = -distY * (toFloat rows - 1) / 2
  in
    map (((+) yOff ) . (*) distY) [0..rows-1] |> map cardBoxRow |> concat


-- todo: shuffle effects
cards : Cards
cards =
  let
    effects = [ Lissajous.make
              , Plasma.make
              , Particles.make
              , Sinescroller.make
              , Starfield.make
              , Tunnel.make ]
  in
    zipWith (\effect box -> Card.make effect box) (effects ++ effects) cardBoxes



data State = Start | Play | Won



type Game = { state:State
            , cards:Cards
            , time:Time }

defaultGame : Game
defaultGame =
  { state = Start
  , cards = cards
  , time = 0 }


-- /---------\
-- | updates |
-- \---------/

{-| Since the game is always scaled maximally into the window
(keeping its aspect ratio), the mouse and touch positions
have to be converted to game positions. -}
winPosToGamePos : Positioned a -> (Int,Int) -> Point
winPosToGamePos pos size =
  let
    intPairToFloatPair (a, b) = (toFloat a, toFloat b)
    (winX, winY) = (pos.x, pos.y)
    (sizeX, sizeY) = intPairToFloatPair size
    (middleX, middleY) = (sizeX / 2, sizeY / 2)
    factor = gameScale size (gameWidth,gameHeight)
  in
    point ((winX - middleX) / factor) ((middleY - winY) / factor)

{-| Calculate factor by which the game is scaled visually onto the screen. -}
gameScale : (Int,Int) -> (Float,Float) -> Float
gameScale (winW, winH) (gameW,gameH) =
  min (toFloat winW / gameW) (toFloat winH / gameH)

gameState : Signal Game
gameState = foldp stepGame defaultGame input

inBox : Positioned a -> Boxed b -> Bool
inBox pos box =
  let
    xDist = abs (pos.x - box.x)
    yDist = abs (pos.y - box.y)
  in
    xDist <= box.w/2 && yDist <= box.h/2

goflipCardsHit : Point -> Card -> Cards -> Cards
goflipCardsHit tapPos ({status} as card) acc =
  let
    hit = tapPos `inBox` card
    status' = if not hit then status else case status of
                                            Card.Done -> Card.Done
                                            Card.FaceUp -> Card.FaceDown
                                            Card.FaceDown -> Card.FaceUp
    card' = { card | status <- status' }
  in
    card'::acc

flipCardsHit : Point -> Cards -> Cards
flipCardsHit tapPos cards =
    foldr (goflipCardsHit tapPos) [] cards

allEqual : Cards -> Bool
allEqual cards =
  let
    es = map .effect cards
  in
    if length es < 2 then True
      else all (equalType (head es)) (tail es)

-- todo: simplify
stepTap : Point -> Game -> Game
stepTap gameTapPos ({state,cards} as game) =
  let
    (cardsDone, cardsNotDone) = partition (((==) Card.Done) . (.status)) cards
    cardsNotDoneFaceDown = map (\c -> {c | status <- Card.FaceDown}) cardsNotDone
    faceUpCount = cards |> filter (((==) Card.FaceUp) . (.status)) |> length
    cardsNotDone' = flipCardsHit gameTapPos <|
                      if faceUpCount == 2 then cardsNotDoneFaceDown
                                          else cardsNotDone
    (cardsNotDoneUp', cardsNotDoneDown') =
       partition (((==) Card.FaceUp) . (.status)) cardsNotDone'
    foundPair = length cardsNotDoneUp' == 2 && allEqual cardsNotDoneUp'
    cardsNotDoneUpDone' = map (\c -> {c | status <- Card.Done}) cardsNotDoneUp'
    allDone = all
    cards' = cardsDone ++ cardsNotDoneDown'
             ++ if foundPair then cardsNotDoneUpDone' else cardsNotDoneUp'
    state' = case state of
               Won -> Won
               Start -> Play
               Play -> Play
               Play -> if all (((==) Card.Done) . .status) cards' then Won
                                                                  else Play
  in
    { game | state <- state',
             cards <- cards' }



stepCards : Float -> Cards -> Cards
stepCards delta cards = map (Card.step delta) cards

stepDelta : Float -> Game -> Game
stepDelta delta ({cards, state, time} as game) =
  let
    cards' = stepCards delta cards
  in
    { game | cards <- cards',
             time <- case state of
                       Play -> time + delta
                       _ -> time }

stepGame : Input -> Game -> Game
stepGame ({action}) ({state, time} as game) =
  case action of
    Step delta -> stepDelta delta game
    Tap tapPos winDims -> stepTap (winPosToGamePos tapPos winDims) game





-- /---------\
-- | display |
-- \---------/

displayCard : Time -> Card -> Form
displayCard time card =
  let
    f (Effect ef) = ef.display
    texture = case card.status of
                  Card.FaceDown -> group [ Card.backside time, Card.border ]
                  --FaceDown -> card.effect.display
                  Card.FaceUp -> group [ f card.effect, Card.border ]
                  --Done -> group [f card.effect, doneOverlay time]
                  Card.Done -> rect 0 0 |> filled (rgb 0 0 0)
  in
    texture |> move (card.x, card.y) |> scale (card.w / 200)


displayCards : Time -> Cards -> Form
displayCards time cards =
  map (displayCard time) cards |> group

{-| Render text using a given transformation function. -}
txt : (Text -> Text) -> String -> Element
txt f = text . f . monospace . Text.color lightBlue . toText

{-| Draw game into a form with size (gameWidth,gameHeight). -}
display : Game -> Form
display ({state,time,cards} as game) =
  let
    timeTextForm = txt (Text.height timeTextHeight) (show <| time / 1000)
                     |> toForm |> move (0, timeTextPosY)
  in
    group
      [
        displayCards time cards
        , timeTextForm
      ]

{-| Draw game maximized into the window. -}
displayFullScreen : (Int,Int) -> Game -> Element
displayFullScreen (w,h) game =
  let
    factor = gameScale (w,h) (gameWidth,gameHeight)
  in
    collage w h [ display game |> scale factor ]

main = displayFullScreen <~ Window.dimensions ~ gameState