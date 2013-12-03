module EulerSpiral where

{-| Generates a euler spiral effect.

@docs eulerSpiral
-}

-- todo: like vector ball in second reality,
--       very slow and dark starfield in background
--       shadows.

import Common(Positioned,Vector,vector,angle2D,vector2DFromAngle,
              multVec,addVec,subVec)
import Effect(Effect, effect)
import Effect
import Starfield

type State = {time:Float, background:Effect, points:[Vector], stepCount:Int}

eulerSpiral : State -> Effect
eulerSpiral s = Effect {step = step s, display = display s, name = "Lissajous"}

make : Effect
make = eulerSpiral { time=0, background=Starfield.make Starfield.BW 0.3 64
                   , points=[vector 0 0 0, vector 10 0 0]
                   , stepCount=0 }

addPoint : [Vector] -> Int -> [Vector]
addPoint ((a::b::_) as points) stepCount =
  let
    d = a `subVec` b
    angle : Float
    angle = angle2D d
    angle' = angle + toFloat stepCount / 8.34567
    d' = vector2DFromAngle angle'
    d'' = d' `multVec` 4
    p' = a `addVec` d''
  in
    p'::points |> take 512


step : State -> Float -> Effect
step ({time,background,points,stepCount} as state) delta =
  eulerSpiral { state | time <- time + delta
                      --, background <- Effect.step background delta
                      , points <- addPoint points stepCount
                      , stepCount <- stepCount + 1}

getPointRange : [Vector] -> ((Float,Float),(Float,Float))
getPointRange points =
  let
    minX = foldl1 min <| map .x points
    minY = foldl1 min <| map .y points
    maxX = foldl1 max <| map .x points
    maxY = foldl1 max <| map .y points
  in
    ((minX,maxX),(minY,maxY))

displayLines : [Vector] -> Form
displayLines points =
  let
    width = 1
    --lS1 = solid (hsva (2.6 + cos num) 1 1 0.1)
    lS1 = solid black
    lS1Wide = { lS1 | width <- width, join <- Smooth, cap <- Round }
    pointToPair {x,y} = (x,y)
    outline = map pointToPair points |> path
  in
    outline |> traced lS1Wide

{-| Returns a euler spiral effect filled form depending on the current time. -}
display : State -> Form
display ({time,background,points} as state) =
  let
    rawForm = displayLines points
    ((minX,maxX),(minY,maxY)) = getPointRange points
    cX = (minX + maxX) / 2
    cY = (minY + maxY) / 2
    w = maxX - minX
    h = maxY - minY
    maxEdge = max w h
    scaleFactor = 180 / maxEdge
  in
    rawForm |> move (-cX*scaleFactor, -cY*scaleFactor) |> scale scaleFactor
  --Effect.display background
  --group [ rect 200 200 |> filled (rgb 255 0 255) ]