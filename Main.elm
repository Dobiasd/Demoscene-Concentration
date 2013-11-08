module DemosceneConcentration where

{-| The classical memory game with old school demoscene effects.

The game begins when the player turns over
the first card. Maximally two cards at a time can be turned over.
If a pair it found, it stays image up. After all pairs are found the
needed time inversly indicates the players performance. ;-)
-}

import Touch
import Window


-- /---------------------\
-- | model configuration |
-- \---------------------/

{-| The game field extends from -100 to +100 in x and y coordinates. -}
(gameWidth,gameHeight) = (200,200)
framesPerSecond = 60
effects = []


-- /--------------------\
-- | view configuration |
-- \--------------------/

manualText = "Guide the ball safely to its goal (green)."
respawnText = "Please go to the start (yellow) to begin/respawn."
timeTextHeight = 7
timeTextPosY = 95
textHeight = 5
textPosY = -90


-- /--------\
-- | inputs |
-- \--------/

data Action = Tap Point | Step Float
type Input = { winSize:(Int,Int), action:Action }

speed : Signal Time
speed = fps framesPerSecond

input : Signal Input
input = (Input <~ Window.dimensions ~ actions)

steps : Signal Action
steps = lift Step speed

flips : Signal Action
flips =
  let
    f t = Tap <| point (toFloat t.x) (toFloat t.y)
  in
    lift f Touch.taps |> dropRepeats

actions = merge steps flips


-- /-------\
-- | model |
-- \-------/

type Positioned a = { a | x:Float, y:Float }
type Sized      a = { a | w:Float, h:Float }
type Boxed      a = Sized (Positioned a)
type Point = Positioned {}
type Effect = Int -- todo function
type Card = Boxed ({effect:Effect})
type Cards = [Card]
cards = []

data State = Start | Play | Won

point : Float -> Float -> Point
point x y = {x=x, y=y}

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
winPosToGamePos : (Int,Int) -> (Int,Int) -> (Float, Float)
winPosToGamePos pos size =
  let
    intPairToFloatPair (a, b) = (toFloat a, toFloat b)
    (winX, winY) = intPairToFloatPair pos
    (sizeX, sizeY) = intPairToFloatPair size
    (middleX, middleY) = (sizeX / 2, sizeY / 2)
    factor = gameScale size (gameWidth,gameHeight)
  in
    ((winX - middleX) / factor, (middleY - winY) / factor)

{-| Calculate factor by which the game is scaled visually onto the screen. -}
gameScale : (Int,Int) -> (Float,Float) -> Float
gameScale (winW, winH) (gameW,gameH) =
  min (toFloat winW / gameW) (toFloat winH / gameH)

gameState : Signal Game
gameState = foldp stepGame defaultGame actions

stepDelta : Time -> Game -> Game
stepDelta delta ({state, time} as game) =
  case state of
    Play -> { game | time <- if state == Start then time else time + delta }
    _ -> game

stepTap : Point -> Game -> Game
stepTap tapPos game = { game | state <- Play }

stepGame : Action -> Game -> Game
stepGame action ({state, time} as game) =
  case action of
    Step delta -> { game | time <- if state == Start
      then time
      else time + delta }
    Tap tapPos -> stepTap tapPos game


-- /---------\
-- | display |
-- \---------/

{-| Draw game into a form with size (gameWidth,gameHeight). -}
display : Game -> Form
display game =
  group
    [
      asText game |> toForm
    ]

{-| Draw game maximized into the window. -}
displayFullScreen : (Int,Int) -> Game -> Element
displayFullScreen (w,h) game =
  let
    factor = gameScale (w,h) (gameWidth,gameHeight)
  in
    collage w h [ display game |> scale factor ]

main = displayFullScreen <~ Window.dimensions ~ dropRepeats gameState