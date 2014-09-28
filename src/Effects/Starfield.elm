module Effects.Starfield where

{-| Generates a starfield effect.
-}

import Effects.Effect
import Common.Vector(Vector, vector, dist, project2d)
import Common.Algorithms(nonOverlappingQuadruples)
import Common.Random(randomFloats)
import Common.Types(Positioned,Colored,Point)
import Common.Display(decomposeColor)

type Star = Positioned (Colored {})

star : Vector -> Color -> Star
star pos col = { pos | col = col }

data Mode = BW | Colored

type State = {time:Float, stars:[Star], mode:Mode, speed:Float, amount:Int}

starfield : State -> Effects.Effect.Effect
starfield s = Effects.Effect.Effect
  {step = step s, display = display s, name = "Starfield"}

make : Mode -> Float -> Int -> Effects.Effect.Effect
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
    randoms = randomFloats time (amount*4)
    triples = nonOverlappingQuadruples randoms
    calcCol v = case mode of
                  BW -> rgb 255 255 255
                  Colored -> hsl (v*123.234) 1 0.5
    f (x,y,z,c) = star (vector (100*x - 50)
                               (100*y - 50)
                               (80*z  - 180))
                       (calcCol c)
  in
    map f triples

stepStar : Float -> Star -> Star
stepStar d ({z} as star) = { star | z <- z + d }

stepStars : Float -> [Star] -> [Star]
stepStars delta = map (stepStar delta)

step : State -> Float -> Effects.Effect.Effect
step ({time, stars, speed, mode, amount} as state) delta =
  let
    oldStars = stars |> (stepStars (speed * delta)) |> filter starInAllowedRange
    newAmount = max 0 (amount - length oldStars)
    stars' = (generateNewStars mode) newAmount time ++ oldStars
  in
    starfield { state | time <- time + delta
                      , stars <- stars' }

displayStar : Star -> Form
displayStar ({x,y,z,col} as star) =
  let
    (r,g,b,a) = decomposeColor col
    radius = 100 / (-z)
    intensity = 1 / (sqrt(sqrt(sqrt(-z))))
    grad = radial (0,0) 0 (0,0) radius
                  [ (0 ,rgba r g b intensity)
                  , (1 ,rgba r g b 0) ]
    pos2d = project2d star
  in
    circle radius |> gradient grad |> move (pos2d.x,pos2d.y)

display : State -> Form
display ({time, stars} as state) =
  let
    backGround = rect 200 200 |> filled (rgb 0 0 0)
    starForms = map displayStar stars
  in
    backGround :: starForms |> group