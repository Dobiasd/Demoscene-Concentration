module Common.Display where

{-| Exports functions and types that can come in handy when displaying stuff.
-}

import Common.Types(Positioned,Point,point2D)
import Common.Algorithms(sortByLess)

import Color(Color, toRgb)
import Graphics.Element(Element)
import Graphics.Collage(Form, move, group, collage, scale)
import List(filter, map)
import Time(Time)

(gameWidth,gameHeight) = (200,200)

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
    point2D ((winX - middleX) / factor) ((middleY - winY) / factor)

{-| Calculate factor by which the game is scaled visually onto the screen. -}
gameScale : (Int,Int) -> (Float,Float) -> Float
gameScale (winW, winH) (gameW, gameH) =
  min (toFloat winW / gameW) (toFloat winH / gameH)

{-| Split a color into its components. -}
decomposeColor : Color -> (Int,Int,Int,Float)
decomposeColor col =
  let toTuple {red, green, blue, alpha} = (red, green, blue, alpha)
  in toRgb col |> toTuple

{-| Check if a position is inside the allowed box of
(-100,-100),(100,100)). -}
isPosOK : Positioned a -> Bool
isPosOK {x,y,z} = z < -1 && x >= -100 && x <= 100 && y >= -100 && y <= 100

{-| Counts frames and remembers how much where there during the last
passed second. -}
type alias FPSCounter = { time : Time
                        , lastVal : Int
                        , counter : Int }

makeFPSCounter : FPSCounter
makeFPSCounter = { time = 0
                 , lastVal = 0
                 , counter = 0}

{-| Update the FPSCounter.
Saves the current count and resets the counter if a new second began. -}
stepFPSCounter : Time -> FPSCounter -> FPSCounter
stepFPSCounter delta ({time,lastVal,counter} as fpsCounter) =
  let
    time' = time + delta
    counter' = counter + 1
  in
    if time' < 1000 then { fpsCounter | time <- time'
                                      , counter <- counter + 1 }
                    else { fpsCounter | time <- time' - 1000
                                      , lastVal <- counter
                                      , counter <- 0 }

{-| A form with a position for later sorting/filtering. -}
type alias PositionedForm = Positioned {f:Form}
positionedForm : Form -> Positioned a -> PositionedForm
positionedForm f {x,y,z} = { f=f, x=x, y=y, z=z }

displayPositionedForm : PositionedForm -> Form
displayPositionedForm {f,x,y} = f |> move (x, y)

{-| Filter out forms with invalid positions,
sort forms by their z coordinates (since we have no z buffer)
and display them. -}
displayPositionedForms : List PositionedForm -> Form
displayPositionedForms fs =
  fs
  |> filter isPosOK
  |> sortByLess (\a b -> a.z > b.z)
  |> map displayPositionedForm |> group

{-| Draw game maximized into the window. -}
displayFullScreen : (a -> Form) -> a -> (Int,Int) -> Element
displayFullScreen displayFunc state (w,h) =
  let
    factor = gameScale (w,h) (gameWidth,gameHeight)
  in
    collage w h [ displayFunc state |> scale factor ]