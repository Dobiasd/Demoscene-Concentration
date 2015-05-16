module Effects.EulerSpiral where

{-| Generates a euler spiral effect.
-}

import Color exposing (hsla, rgb)
import Graphics.Collage exposing (solid, LineCap(Round), path, traced, Form
  , group, rect, filled, move, scale)
import List exposing ((::), take, map, reverse)

import Common.Algorithms exposing (numberedPairs,uncurry3,unsafeMaybe)
import Common.Types exposing (Positioned)
import Common.Vector exposing (Vector,vector,angle2D,vector2DFromAngle,
              multVec,addVec,subVec)
import Effects.Effect as Eff

type alias State = {time:Float, points:List Vector, stepCount:Int}

eulerSpiral : State -> Eff.Effect
eulerSpiral s =
  Eff.Effect {step=step s, display=display s, name="Lissajous"}

make : Eff.Effect
make = eulerSpiral { time=0
                   , points=[vector 0 0 0, vector 10 0 0]
                   , stepCount=0 }

addPoint : List Vector -> Int -> List Vector
addPoint ((a::b::_) as points) stepCount =
  let
    angle = angle2D <| a `subVec` b
    angle' = angle + toFloat stepCount / 8.34567
    d = vector2DFromAngle angle'
    d' = d `multVec` 4
    p = a `addVec` d'
  in
    p::points |> take 512

step : State -> Float -> Eff.Effect
step ({time,points,stepCount} as state) delta =
  eulerSpiral { state | time <- time + delta
                      , points <- addPoint points stepCount
                      , stepCount <- stepCount + 1}

getPointRange : List Vector -> ((Float,Float),(Float,Float))
getPointRange points =
  let
    minX = map .x points |> List.minimum |> unsafeMaybe
    minY = map .y points |> List.minimum |> unsafeMaybe
    maxX = map .x points |> List.maximum |> unsafeMaybe
    maxY = map .y points |> List.maximum |> unsafeMaybe
  in
    ((minX,maxX),(minY,maxY))

displayLine : Float -> Int -> Vector -> Vector -> Form
displayLine time num s e =
  let
    width = 2
    lS1 = solid (hsla (toFloat num / 10 + time / 1000) 1 0.5 1)
    lS1Wide = { lS1 | width <- width, cap <- Round }
    pointToPair {x,y} = (x,y)
    outline = [pointToPair s, pointToPair e] |> path
  in
    outline |> traced lS1Wide

displayLines : List Vector -> Float -> List Form
displayLines points time =
  reverse points |> numberedPairs |> map (uncurry3 (displayLine time))

display : State -> Form
display ({time,points} as state) =
  let
    rawForm = displayLines points time |> group
    ((minX,maxX),(minY,maxY)) = getPointRange points
    cX = (minX + maxX) / 2
    cY = (minY + maxY) / 2
    w = maxX - minX
    h = maxY - minY
    maxEdge = max w h
    scaleF = 180 / maxEdge
  in
    group [ rect 200 200 |> filled (rgb 0 0 0)
          , rawForm |> move (-cX*scaleF, -cY*scaleF) |> scale scaleF ]