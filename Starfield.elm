module Starfield where

{-| Generates a starfield effect.

@docs starfield
-}

-- todo: configurable speed and color

import Effect(Effect, effect)
import Common(Vector, vector, nonOverlappingQuadruples, dist, randoms,
              Positioned3,Colored,Point,point)

type Star = Positioned3 (Colored {})

star : Vector -> Color -> Star
star pos col = { pos | col = col }

data Mode = BW | Colored

type State = {time:Float, stars:[Star], mode:Mode, speed:Float, amount:Int}

starfield : State -> Effect
starfield s = Effect {step = step s, display = display s, name = "Starfield"}

make : Mode -> Float -> Int -> Effect
make mode speed amount =
  starfield {time=0, stars=[], mode=mode, speed=speed, amount=amount}

minDist = 2

starInAllowedRange : Star -> Bool
starInAllowedRange ({x,y,z} as star) =
  let
    pos2d = project2d star
  in
    z < (-minDist) &&
    dist (vector x y 0) > 10 &&
    pos2d.x >= -100 && pos2d.x <= 100 &&
    pos2d.y >= -100 && pos2d.y <= 100

generateNewStars : Mode -> Int -> Float -> [Star]
generateNewStars mode amount time =
  let
    randomFloats = randoms time (amount*4)
    triples = nonOverlappingQuadruples randomFloats
    calcCol v = case mode of
                  BW -> rgb 255 255 255
                  Colored -> hsv (v*123.234) 1 1
    f (x,y,z,c) = star (vector (10000*x - 5000)
                               (10000*y - 5000)
                               (80*z  - 180))
                     (calcCol c)
  in
    map f triples

stepStar : Float -> Star -> Star
stepStar delta ({z} as star) = { star | z <- z + 0.05 * delta }

stepStars : Float -> [Star] -> [Star]
stepStars delta = map (stepStar delta)

step : State -> Float -> Effect
step ({time, stars, speed, mode, amount} as state) delta =
  let
    oldStars = stars |> (stepStars (speed * delta)) |> filter starInAllowedRange
    newAmount = max 0 (amount - length oldStars)
    stars' = oldStars ++ (generateNewStars mode) newAmount time
  in
    starfield { state | time <- time + delta
                      , stars <- stars' }

project2d : Positioned3 a -> Point
project2d {x,y,z} = point (x / (-z)) (y / (-z))

decomposeColor : Color -> (Int,Int,Int,Float)
decomposeColor (Color r g b a) = (r,g,b,a)

displayStar : Star -> Form
displayStar ({x,y,z,col} as star) =
  let
    (r,g,b,a) = decomposeColor col
    radius = 100 / (-z)
    intensity = 1 / (sqrt(sqrt(sqrt(-z))))
    grad = radial (0,0) 0 (0,0) radius
          [(0 ,rgba r g b intensity),
           (1 ,rgba r g b 0)]
    pos2d = project2d star
  in
    circle radius |> gradient grad |> move (pos2d.x,pos2d.y)

{-| Returns a starfield effect filled form depending on the current time. -}
display : State -> Form
display ({time, stars} as state) =
  let
    backGround = rect 200 200 |> filled (rgb 0 0 0) -- todo: make black
    starForms = map displayStar stars
  in
    backGround :: starForms |> group