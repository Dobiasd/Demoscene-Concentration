module DemosceneConcentration where

{-| The classical memory game with old school demoscene effects.

The game begins when the player turns over
the first card. Maximally two cards at a time can be turned over.
If a pair it found, it stays image up. After all pairs are found the
needed time inversly indicates the players performance. ;-)
-}

import Touch
import Window

import Effects


-- /---------------------\
-- | model configuration |
-- \---------------------/

{-| The game field extends from -100 to +100 in x and y coordinates. -}
(gameWidth,gameHeight) = (200,200)
framesPerSecond = 60


-- /--------------------\
-- | view configuration |
-- \--------------------/

timeTextHeight = 7
timeTextPosY = 95


-- /--------\
-- | inputs |
-- \--------/

data CardStatus = FaceDown | FaceUp | Done
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

type Named      a = { a | name:String }
type Positioned a = { a | x:Float, y:Float }
type Sized      a = { a | w:Float, h:Float }
type Boxed      a = Sized (Positioned a)

type Point = Positioned {}
type Box = Boxed {}

type Card = Boxed {effect:Effects.Effect, status:CardStatus}
type Cards = [Card]

backside : Time -> Form
backside _ = rect 200 200 |> filled (rgb 0 0 0)

doneOverlay : Time -> Form
doneOverlay _ = rect 200 200 |> filled (rgba 0 0 0 0.5)

{-| Creation of one single row of cards with equidistant gaps. -}
cardBoxRow : Float -> [Box]
cardBoxRow y =
  let
    cols = 4
    distX = 50
    xOff = -distX * (toFloat cols - 1) / 2
    cardWidth = 45
    cardHeight = 45
  in
    map (\x -> box (distX * x + xOff) y cardWidth cardHeight) [0..cols-1]

cardBoxes =
  let
    rows = 3
    distY = 55
    yOff = -distY * (toFloat rows - 1) / 2
  in
    map (((+) yOff ) . (*) distY) [0..rows-1] |> map cardBoxRow |> concat

card effect box =
  let boxedEffect = { box | effect=effect }
  in { boxedEffect | status=FaceDown }



-- todo: shuffle effects
cards : Cards
cards =
  let
    effects = [ Effects.makePlasma
              , Effects.makeStarfield
              , Effects.makeParticles
              , Effects.makeTunnel
              , Effects.makeLissajous
              , Effects.makeSinescroller ]
  in
    zipWith (\effect box -> card effect box) (effects ++ effects) cardBoxes



data State = Start | Play | Won

point : Float -> Float -> Point
point x y = {x=x, y=y}

box : Float -> Float -> Float -> Float -> Box
box x y w h = {x=x, y=y, w=w, h=h }

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
                                            Done -> Done
                                            FaceUp -> FaceDown
                                            FaceDown -> FaceUp
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
      else all (Effects.equalType (head es)) (tail es)

-- todo: simplify
stepTap : Point -> Game -> Game
stepTap gameTapPos ({state,cards} as game) =
  let
    (cardsDone, cardsNotDone) = partition (((==) Done) . (.status)) cards
    cardsNotDoneFaceDown = map (\c -> {c | status <- FaceDown}) cardsNotDone
    faceUpCount = cards |> filter (((==) FaceUp) . (.status)) |> length
    cardsNotDone' = flipCardsHit gameTapPos <|
                      if faceUpCount == 2 then cardsNotDoneFaceDown
                                          else cardsNotDone
    (cardsNotDoneUp', cardsNotDoneDown') =
       partition (((==) FaceUp) . (.status)) cardsNotDone'
    foundPair = length cardsNotDoneUp' == 2 && allEqual cardsNotDoneUp'
    cardsNotDoneUpDone' = map (\c -> {c | status <- Done}) cardsNotDoneUp'
    allDone = all
    cards' = cardsDone ++ cardsNotDoneDown'
             ++ if foundPair then cardsNotDoneUpDone' else cardsNotDoneUp'
    state' = case state of
               Won -> Won
               Start -> Play
               Play -> if all (((==) Done) . .status) cards' then Won
                                                             else Play
  in
    { game | state <- state',
             cards <- cards' }

stepCard : Float -> Card -> Card
stepCard delta ({status, effect} as card) =
  case status of
    FaceUp -> { card | effect <- Effects.step delta effect }
    _ -> card

stepCards : Float -> Cards -> Cards
stepCards delta cards = map (stepCard delta) cards

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
  let texture = case card.status of
                  FaceDown -> backside time
                  --FaceDown -> Effects.display card.effect
                  FaceUp -> Effects.display card.effect
                  Done -> group [Effects.display card.effect, doneOverlay time]
  in texture |> move (card.x, card.y) |> scale (card.w / 200)


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
        , asText game |> toForm |> scale 0.2
        --, asText cards |> toForm |> scale 0.2
      ]

{-| Draw game maximized into the window. -}
displayFullScreen : (Int,Int) -> Game -> Element
displayFullScreen (w,h) game =
  let
    factor = gameScale (w,h) (gameWidth,gameHeight)
  in
    collage w h [ display game |> scale factor ]

main = displayFullScreen <~ Window.dimensions ~ dropRepeats gameState