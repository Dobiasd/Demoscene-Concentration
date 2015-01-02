module DemosceneConcentration where

{-| The classical memory game with simple old school demoscene effects.

The game begins when the player turns over the first card.
Maximally two cards at a time can be turned over.
If a pair it found, it is removed. After all pairs are found the
needed time inversly indicates the players performance. ;-)

Every card carries an effect with it, which is only updated
when the front side is shown.
-}


import Color(rgb, lightBlue)
import Graphics.Collage(group, Form, toForm, move)
import Graphics.Element(Element)
import List(map, concat, length, map2, (::), foldr, filter, all, isEmpty)
import Signal
import Signal(merge, Signal, (<~), (~), sampleOn, dropRepeats, foldp)
import Text(Text)
import Text
import Time(Time, fps)
import Touch
import Window

import Common.Types(Point,Positioned,Boxed,Box,point2D,box2D,inBox)
import Common.Random(randomInts,shuffle)
import Common.Display(winPosToGamePos,displayFullScreen,
                      FPSCounter,makeFPSCounter,stepFPSCounter)
import Card
import Card(Card,Cards,allEqual,splitCardsByStatus)

import Effects.Effect as Eff

import Effects.Cube as Cube
import Effects.EulerSpiral as EulerSpiral
import Effects.Moire as Moire
import Effects.Plasma as Plasma
import Effects.Particles as Particles
import Effects.Sinescroller as Sinescroller
import Effects.Tunnel as Tunnel


-- /--------\
-- | inputs |
-- \--------/

{-| Input actions can indicate a user input for the game locic
or a time step for the animations. -}

type alias Input = { action:Action }

type Action = Tap Point (Int,Int) | Step Float

actions = merge steps flips

speed : Signal Time
speed = fps 60

input : Signal Input
input = (Input <~ actions)

steps : Signal Action
steps = Signal.map Step speed

flips : Signal Action
flips =
  let
    f t winDims = Tap (point2D (toFloat t.x) (toFloat t.y)) winDims
  in
    f <~ Touch.taps ~ Window.dimensions |> sampleOn Touch.taps |> dropRepeats


-- /-------\
-- | model |
-- \-------/

{-| The game field extends from -100 to +100 in x and y coordinates. -}

{-| Create one single row of boxes with equidistant gaps. -}
boxRow : Float -> List Box
boxRow y =
  let
    cols = 4
    distX = 50
    xOff = -distX * (toFloat cols - 1) / 2
    cardWidth = 45
    cardHeight = 45
    createBox x = box2D (distX * x + xOff) (y-2) cardWidth cardHeight
  in
    map createBox [0..cols-1]

{-| Create boxes nicely spaced boxes representing the cards
positions and sized. -}
cardBoxes : List Box
cardBoxes =
  let
    rows = 3
    distY = 55
    yOff = -distY * (toFloat rows - 1) / 2
  in
    map ((*) distY >> ((+) yOff)) [0..rows-1] |> map boxRow |> concat

{-| The Effects we want to use for our cards.
Every one will of occur two times. -}
effects : List Eff.Effect
effects = [ EulerSpiral.make
          , Plasma.make
          , Particles.make
          , Sinescroller.make "Greetings go out to everybody loving the demoscene and functional programming. ;-)   -   daiw.de"
          , Moire.make
          , Tunnel.make ]

{-| Generate the list of shuffled cards on the table. -}
generateCards : Float -> Cards
generateCards seed =
  let
    numbers = randomInts (round seed) (length cardBoxes + 1)
    shuffledBoxes = shuffle numbers cardBoxes
  in
    map2 (\effect box -> Card.make effect box)
         (effects ++ effects)
         shuffledBoxes

{-| The whole game state. -}
type alias Game = { state:State
                  , cards:Cards
                  , wonEffect:Eff.Effect
                  , time:Time
                  , fpsCounter:FPSCounter }

type State = Start | Play | Won

{-| Initial game. -}
defaultGame : Game
defaultGame =
  { state = Start
  , cards = generateCards 0 -- Will be shuffled later.
  , wonEffect = Cube.make [] (rgb 0 0 0) -- Will be filled later.
  , time = 0
  , fpsCounter = makeFPSCounter }


-- /---------\
-- | updates |
-- \---------/

gameState : Signal Game
gameState = foldp stepGame defaultGame input

{-| If one card was hit by a tap, flip it.
Otherwise leave everything as it is. -}
flipCardsHit : Point -> Cards -> Cards
flipCardsHit tapPos cards =
  let
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
  in
    foldr (goflipCardsHit tapPos) [] cards

{-| If the game is not started yet and the player tabs the screen
the cards are shuffled initially. -}
stepTapStart : Point -> Game -> Game
stepTapStart ({x,y} as gameTapPos) ({state,cards} as game) =
  let
    shuffledCards = generateCards (x+y)
  in
    stepTap gameTapPos { game | state <- Play,
                                cards <- shuffledCards }

{-| Generate the final effect to be shown when the game is solved. -}
-- The Sinescroller is removed here, because its Text will display the
-- needed time. Since this is possible only after the game is finished,
-- it will be added then.
generateWonEffect : Time -> Eff.Effect
generateWonEffect time =
  let
    message = toString (roundTime time) ++ " seconds"
    wonEffects = filter (\(Eff.Effect e) -> e.name /= "Sinescroller") effects
  in
    Cube.make (Sinescroller.make message :: wonEffects) (rgb 64 64 64)

{-| If the game is running and the player tabs the screen
some cards will possibly get flipped. -}
stepPlayFlipCards : Point -> Cards -> Cards
stepPlayFlipCards tapPos cards =
  let
    -- Only the cards that are not yet done play a role in flipping.
    cardsNotDone = filter (not << ((==) Card.Done) << (.status)) cards
    -- Put given cards face down.
    putFaceDown = map (\c -> {c | status <- Card.FaceDown})
    -- How many cards are there face up?
    faceUpCount = cards |> filter (((==) Card.FaceUp) << (.status)) |> length
  in
    -- If two cards are face up, they are turned down again
    -- before another card can be turned up.
    flipCardsHit tapPos <| if faceUpCount == 2 then putFaceDown cardsNotDone
                                               else cardsNotDone

{-| The game is running and the player tabs the screen.
If a card is tabbed it will be turned.
If one not hit card is already face up, nothing happens with it.
If two not hit cards are face up, they are turned face down.
If a pair is found, both involved cards are set to done. -}
stepTapPlay : Point -> Game -> Game
stepTapPlay gameTapPos ({cards,time} as game) =
  let
    -- Return only already done cards.
    getDoneCards = filter (((==) Card.Done) << (.status))
    -- Not done cards after possible flips.
    cardsNotDone' = stepPlayFlipCards gameTapPos cards
    -- In game cards splitted by state.
    (cardsNotDoneUp', cardsNotDoneDown') = splitCardsByStatus Card.FaceUp
                                                              cardsNotDone'
    -- Did the player uncover a pair?
    foundPair = length cardsNotDoneUp' == 2 && Card.allEqual cardsNotDoneUp'
    putCardsFaceDown = map (\c -> {c | status <- Card.Done})
    cards' = getDoneCards cards ++ cardsNotDoneDown'
             ++ if foundPair then putCardsFaceDown cardsNotDoneUp'
                             else cardsNotDoneUp'
  in
    { game | cards <- cards'
           , wonEffect <- generateWonEffect time }

{-| Dispatch user input by game state. -}
stepTap : Point -> Game -> Game
stepTap gameTapPos ({state,cards} as game) =
  case state of
    Start -> stepTapStart gameTapPos game
    Play -> stepTapPlay gameTapPos game
    _ -> game

{-| Update card's animation and remove gone cards. -}
stepCards : Float -> Cards -> Cards
stepCards delta cards =
  map (Card.step delta) cards |> filter (not << Card.isGone)

{-| Update wonEffect (only used when game is already solved). -}
stepWon : Float -> Game -> Game
stepWon delta ({wonEffect} as game) =
  { game | wonEffect <- Eff.step wonEffect delta }

{-| Calculate state for next frame. -}
stepDelta : Float -> Game -> Game
stepDelta delta ({cards, state, time, fpsCounter} as game) =
  let
    -- If all cards are done the stopwatch is stopped.
    allDone = all (((==) Card.Done) << .status) cards
    time' = case state of
              Play -> if allDone then time
                                 else time + delta
              _    -> time
    -- Step game according to state
    game' = case state of
      Won -> stepWon delta game
      _   -> { game | cards <- stepCards delta cards
                    , time  <- time'
                    , state <- if isEmpty cards then Won else state }
  in
    { game' | fpsCounter <- stepFPSCounter delta fpsCounter }

{-| Dispatch by inpout. -}
stepGame : Input -> Game -> Game
stepGame ({action}) ({state, time} as game) =
  case action of
    Step delta -> stepDelta delta game
    Tap tapPos winDims -> stepTap (winPosToGamePos tapPos winDims) game


-- /---------\
-- | display |
-- \---------/

{-| Dispatch by input. -}
displayCards : Time -> Cards -> Form
displayCards time cards =
  map (Card.display time) cards |> group

{-| Render text using a given transformation function. -}
txt : (Text -> Text) -> String -> Element
txt f =
  Text.fromString
  >> Text.color lightBlue
  >> Text.monospace
  >> f
  >> Text.leftAligned

{-| Show number of calculated frames during last passed second. -}
displayFPS : FPSCounter -> Form
displayFPS {lastVal} =
  let
    (fpsTextPosX,fpsTextPosY) = (-84,95)
    fpsTextHeight = 7
  in
    txt (Text.height fpsTextHeight) ("FPS: " ++ (toString <| lastVal))
      |> toForm |> move (fpsTextPosX, fpsTextPosY)

{-| Show the final Effect -}
displayWon : Game -> Form
displayWon ({wonEffect} as game) = Eff.display wonEffect

{-| Round time for decisecond precision. -}
roundTime : Time -> Time
roundTime time = (round >> toFloat) (time / 100) / 10

{-| Draw game into a form with size (gameWidth,gameHeight). -}
displayNotYetDone : Game -> Form
displayNotYetDone ({time,cards} as game) =
  let
    timeTextHeight = 10
    timeTextPosY = 90
    timeTextForm = txt (Text.height timeTextHeight) (toString <| roundTime time)
                     |> toForm |> move (0, timeTextPosY)
  in
    group [ displayCards time cards
          , timeTextForm ]

{-| Display game or final effect. FPS are displayed in both cases. -}
display : Game -> Form
display ({state,fpsCounter} as game) =
  group [ case state of
            Won -> displayWon game
            _   -> displayNotYetDone game
        , displayFPS fpsCounter ]

main = (displayFullScreen display) <~ gameState ~ Window.dimensions