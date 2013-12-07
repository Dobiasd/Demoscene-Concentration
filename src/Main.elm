module DemosceneConcentration where

{-| The classical memory game with old school demoscene effects.

The game begins when the player turns over
the first card. Maximally two cards at a time can be turned over.
If a pair it found, it stays image up. After all pairs are found the
needed time inversly indicates the players performance. ;-)
-}

import Touch
import Window

import Effects.Effect(Effect)
import Effects.Effect as Effect

import Common.Types(Point, Positioned, Boxed, Box, point2D, box2D)
import Common.Algorithms(roundTo)
import Common.Random(randomInts,shuffle)
import Common.Display(winPosToGamePos,displayFullScreen)

import Card
import Card(Card)
import Effects.Cube as Cube
import Effects.EulerSpiral as EulerSpiral
import Effects.Moire as Moire
import Effects.Plasma as Plasma
import Effects.Particles as Particles
import Effects.Sinescroller as Sinescroller
import Effects.Tunnel as Tunnel




-- /---------------------\
-- | model configuration |
-- \---------------------/

{-| The game field extends from -100 to +100 in x and y coordinates. -}

framesPerSecond = 60




-- /--------------------\
-- | view configuration |
-- \--------------------/

timeTextHeight = 5
timeTextPosY = 99
fpsTextHeight = 3
fpsTextPosY = 100
fpsTextPosX = -98




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
    f t winDims = Tap (point2D (toFloat t.x) (toFloat t.y)) winDims
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
    map (\x -> box2D (distX * x + xOff) (y-2) cardWidth cardHeight) [0..cols-1]

cardBoxes =
  let
    rows = 3
    distY = 65
    yOff = -distY * (toFloat rows - 1) / 2
  in
    map (((+) yOff ) . (*) distY) [0..rows-1] |> map cardBoxRow |> concat

effects = [ EulerSpiral.make
          , Plasma.make
          , Particles.make
          , Sinescroller.make "Greetings go out to everybody loving the demoscene and functional programming. ;-)   -   daiw.de"
          , Moire.make
          , Tunnel.make ]


wonEffects = filter (\(Effect e) -> e.name /= "Sinescroller") effects

generateCards : Time -> Cards
generateCards time =
  let
    numbers = randomInts (round time) (length cardBoxes + 1)
    shuffledBoxes = shuffle numbers cardBoxes
  in
    zipWith (\effect box -> Card.make effect box) (effects ++ effects) shuffledBoxes


data State = Start | Play | Won

type Game = { state:State
            , cards:Cards
            , wonEffect:Effect
            , time:Time
            , currentFPS:Int }

defaultGame : Game
defaultGame =
  { state = Start
  , cards = generateCards 0
  , wonEffect = Cube.make wonEffects (rgb 64 64 64)
  , time = 0
  , currentFPS = 0 }




-- /---------\
-- | updates |
-- \---------/

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
                                            Card.FaceUp -> Card.FaceDown
                                            Card.FaceDown -> Card.FaceUp
                                            _ -> status
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
    equalName (Effect e1) (Effect e2) = e1.name == e2.name
  in
    if length es < 2 then True
      else all (equalName (head es)) (tail es)

stepTapStart : Point -> Game -> Game
stepTapStart ({x,y} as gameTapPos) ({state,cards} as game) =
  let
    shuffledCards = generateCards (x+y)
  in
    stepTap gameTapPos { game | state <- Play,
                                cards <- shuffledCards }

generateWonEffect : Time -> Effect
generateWonEffect time =
  let
    message = show (roundTime time) ++ " seconds"
  in
    Cube.make (Sinescroller.make message :: wonEffects) (rgb 64 64 64)

stepPlayFlipCards : Point -> Cards -> Cards
stepPlayFlipCards gameTapPos cards =
  let
    cardsNotDone = filter (not . ((==) Card.Done) . (.status)) cards
    cardsNotDoneFaceDown = map (\c -> {c | status <- Card.FaceDown}) cardsNotDone
    faceUpCount = cards |> filter (((==) Card.FaceUp) . (.status)) |> length
  in
   flipCardsHit gameTapPos <| if faceUpCount == 2 then cardsNotDoneFaceDown
                                                  else cardsNotDone

splitCardsByStatus : Card.Status -> Cards -> (Cards,Cards)
splitCardsByStatus status = partition (((==) status) . (.status))

stepTapPlay : Point -> Game -> Game
stepTapPlay gameTapPos ({cards,time} as game) =
  let
    cardsDone = filter (((==) Card.Done) . (.status)) cards
    cardsNotDone' = stepPlayFlipCards gameTapPos cards
    (cardsNotDoneUp', cardsNotDoneDown') = splitCardsByStatus Card.FaceUp cardsNotDone'
    foundPair = length cardsNotDoneUp' == 2 && allEqual cardsNotDoneUp'
    cardsNotDoneUpDone' = map (\c -> {c | status <- Card.Done}) cardsNotDoneUp'
    cards' = cardsDone ++ cardsNotDoneDown'
             ++ if foundPair then cardsNotDoneUpDone' else cardsNotDoneUp'
  in
    { game | cards <- cards'
           , wonEffect <- generateWonEffect time }


stepTap : Point -> Game -> Game
stepTap gameTapPos ({state,cards} as game) =
  case state of
    Start -> stepTapStart gameTapPos game
    Play -> stepTapPlay gameTapPos game
    _ -> game


stepCards : Float -> Cards -> Cards
stepCards delta cards = map (Card.step delta) cards |> filter (not . Card.isGone)

stepWon : Float -> Game -> Game
stepWon delta ({wonEffect} as game) =
  { game | wonEffect <- Effect.step wonEffect delta }

stepDelta : Float -> Game -> Game
stepDelta delta ({cards, state, time} as game) =
  let
    allDone = all (((==) Card.Done) . .status) cards
    time' = case state of
              Play -> if allDone then time
                                 else time + delta
              _    -> time
    game' = case state of
      Won -> stepWon delta game
      _   -> { game | cards <- stepCards delta cards
                    , time  <- time'
                    , state <- if isEmpty cards then Won else state }
  in
    { game' | currentFPS <- round(1000/delta) }

stepGame : Input -> Game -> Game
stepGame ({action}) ({state, time} as game) =
  case action of
    Step delta -> stepDelta delta game
    Tap tapPos winDims -> stepTap (winPosToGamePos tapPos winDims) game





-- /---------\
-- | display |
-- \---------/



displayCards : Time -> Cards -> Form
displayCards time cards =
  map (Card.display time) cards |> group


txt : (Text -> Text) -> String -> Element
txt f = text . f . monospace . Text.color lightBlue . toText

displayFPS : Game -> Form
displayFPS {currentFPS} =
  txt (Text.height fpsTextHeight) ("FPS: " ++ (show <| currentFPS))
                     |> toForm |> move (fpsTextPosX, fpsTextPosY)

displayWon : Game -> Form
displayWon ({wonEffect} as game) = Effect.display wonEffect

roundTime : Time -> Time
roundTime time = (toFloat . round) (time / 100) / 10

{-| Draw game into a form with size (gameWidth,gameHeight). -}
displayNotYetDone : Game -> Form
displayNotYetDone ({time,cards} as game) =
  let

    timeTextForm = txt (Text.height timeTextHeight) (show <| roundTime time)
                     |> toForm |> move (0, timeTextPosY)
  in
    group [ displayCards time cards
          , timeTextForm ]

display : Game -> Form
display ({state} as game) =
  group [
    case state of
      Won -> displayWon game
      _   -> displayNotYetDone game
  , displayFPS game ]


main = (displayFullScreen display) <~ gameState ~ Window.dimensions