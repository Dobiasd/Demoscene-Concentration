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

minZ = 2

-- todo: why the f do I have no stars if this is always True?
starInAllowedRange : Vector -> Bool
starInAllowedRange ({x,y,z} as v) =
  z < (-minZ)
  --dist v > 1000 &&
  --dist (vector x y 0) > 100
  --True

removePassedStars : [Vector] -> [Vector]
removePassedStars = filter starInAllowedRange


generateNewStars : [Vector] -> Time -> [Vector]
generateNewStars stars time =
  let
    amount = max 0 (128 - length stars)
    randomFloats = randoms time amount
    triples = nonOverlappingTriples randomFloats
    f (x,y,z) = vector (8000*x - 4000)
                       (8000*y - 4000)
                       (80*z  - 180)
  in
    map f triples

stepStars : Float -> [Vector] -> [Vector]
stepStars delta = map (\({z} as v) -> { v | z <- z + 0.05 * delta })

step : State -> Float -> Effect
step ({time, stars} as state) delta =
  let
    oldStars = stars |> (stepStars delta) |> removePassedStars
    stars' = oldStars ++ generateNewStars oldStars time
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
    x2d = x / z
    y2d = y / z
  in
    circle radius |> gradient grad |> move (x2d,y2d)

{-| Returns a starfield effect filled form depending on the current time. -}
display : State -> Form
display ({time, stars} as state) =
  let
    backGround = rect 200 200 |> filled (rgb 255 0 0) -- todo: make black
    starForms = map displayStar stars
    --starForms = [displayStar {x=0,y=0,z=-1}]
    -- todo: Why is the number 0 even though is see stars?
    debugForm = asText (length stars) |> toForm
  in
    backGround :: starForms ++ [debugForm] |> group