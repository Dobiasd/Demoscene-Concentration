module Effects.EulerSpiral where

{-| Generates a euler spiral effect.

@docs eulerSpiral
-}


import Common(Positioned,Vector,vector,angle2D,vector2DFromAngle,
              multVec,addVec,subVec,numberedPairs,uncurry3)
import Effects.Effect(Effect, effect)
import Effects.Effect


type State = {time:Float, points:[Vector], stepCount:Int}

eulerSpiral : State -> Effect
eulerSpiral s = Effect {step = step s, display = display s, name = "Lissajous"}

make : Effect
make = eulerSpiral { time=0
                   , points=[vector 0 0 0, vector 10 0 0]
                   , stepCount=0 }

addPoint : [Vector] -> Int -> [Vector]
addPoint ((a::b::_) as points) stepCount =
  let
    angle = angle2D <| a `subVec` b
    angle' = angle + toFloat stepCount / 8.34567
    d = vector2DFromAngle angle'
    d' = d `multVec` 4
    p = a `addVec` d'
  in
    p::points |> take 512

step : State -> Float -> Effect
step ({time,points,stepCount} as state) delta =
  eulerSpiral { state | time <- time + delta
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

displayLine : Float -> Int -> Vector -> Vector -> Form
displayLine time num s e =
  let
    width = 2
    lS1 = solid (hsva (toFloat num / 10 + time / 1000) 1 1 1)
    lS1Wide = { lS1 | width <- width, cap <- Round }
    pointToPair {x,y} = (x,y)
    outline = [pointToPair s, pointToPair e] |> path
  in
    outline |> traced lS1Wide

displayLines : [Vector] -> Float -> Form
displayLines points time =
    group <| map (uncurry3 (displayLine time)) <| numberedPairs <| reverse points

display : State -> Form
display ({time,points} as state) =
  let
    rawForm = displayLines points time
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