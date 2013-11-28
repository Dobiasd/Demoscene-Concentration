module Starfield where

{-| Generates a starfield effect.

@docs starfield
-}

-- todo: configurable speed and color

import Effect(Effect, effect)
import Common(Vector, vector, nonOverlappingTriples, dist, randoms)

type State = {time:Float, stars:[Vector]}

starfield : State -> Effect
starfield s = Effect {step = step s, display = display s, name = "Starfield"}

make : Effect
make = starfield {time=0, stars=[]}

minDist = 2

starInAllowedRange : Vector -> Bool
starInAllowedRange ({x,y,z} as v) = z < (-minDist) && dist (vector x y 0) > 10

generateNewStars : Int -> Float -> [Vector]
generateNewStars amount time =
  let
    randomFloats = randoms time amount
    triples = nonOverlappingTriples randomFloats
    f (x,y,z) = vector (10000*x - 5000)
                       (10000*y - 5000)
                       (80*z  - 180)
  in
    map f triples

stepStars : Float -> [Vector] -> [Vector]
stepStars delta = map (\({z} as v) -> { v | z <- z + 0.05 * delta })

step : State -> Float -> Effect
step ({time, stars} as state) delta =
  let
    oldStars = stars |> (stepStars delta) |> filter starInAllowedRange
    newAmount = max 0 (128 - length oldStars)
    stars' = oldStars ++ generateNewStars newAmount time
  in
    starfield { state | time <- time + delta
                      , stars <- stars' }

displayStar : Vector -> Form
displayStar {x,y,z} =
  let
    radius = 100 / (-z)
    intensity = 1 / (sqrt(sqrt(sqrt(-z))))
    grad = radial (0,0) 0 (0,0) radius
          [(0 ,rgba 255 255 255 intensity),
           (1 ,rgba 255 255 255 0)]
    x2d = x / (-z)
    y2d = y / (-z)
  in
    circle radius |> gradient grad |> move (x2d,y2d)

{-| Returns a starfield effect filled form depending on the current time. -}
display : State -> Form
display ({time, stars} as state) =
  let
    backGround = rect 200 200 |> filled (rgb 0 0 0) -- todo: make black
    starForms = map displayStar stars
  in
    backGround :: starForms |> group