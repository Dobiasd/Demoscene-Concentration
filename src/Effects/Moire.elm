module Effects.Moire where

{-| Generates a moire effect.
-}

import Color exposing (hsla, rgb)
import Graphics.Collage exposing (solid, path, traced, Form, group, rect
  , filled)
import List exposing (map, length, map2)

import Effects.Effect as Eff
import Common.Vector exposing (Vector,vector,Transform3D,applyTransform3D
  ,rotateZ)
import Effects.Starfield as Starfield

type alias State = {time:Float, background:Eff.Effect}

moire : State -> Eff.Effect
moire s = Eff.Effect
  {step = step s, display = display s, name = "Moire"}

make : Eff.Effect
make = moire {time=0, background=Starfield.make Starfield.Colored 0.2 64}

step : State -> Float -> Eff.Effect
step ({time, background} as state) delta =
  moire { state | time <- time + delta
                , background <- Eff.step background delta }

type alias Line = (Vector, Vector)

line : Vector -> Vector -> Line
line s e = (s, e)

pattern : List Line
pattern =
  map (\y -> line (vector (-90*(cos (y/90))) y 0)
                  (vector ( 90*(cos (y/90))) y 0))
    (map ((*)16) [-5..5])

transformLine : Transform3D -> Line -> Line
transformLine m (s,e) =
  line (applyTransform3D m s) (applyTransform3D m e)

displayLine : Float -> Line -> Form
displayLine num (s, e) =
  let
    width = 8
    lS = solid (hsla (2.6 + cos num) 1 0.5 0.1)
    lSWide = { lS | width <- width }
    outline = path [(s.x,s.y), (e.x,e.y)]
  in
    outline |> traced lSWide

displayLines : List Line -> Float -> Form
displayLines pattern angle =
  let
    r = rotateZ angle
    lines = map (transformLine r) pattern
    nums = map toFloat [0..(length(lines))]
  in
    map (uncurry displayLine) (map2 (,) nums lines) |> group

display : State -> Form
display ({time, background} as state) =
  let
    angles = map ((*)(time/20000)) [1..15]
    patterns = map (displayLines pattern) angles |> group
  in
    group [ rect 200 200 |> filled (rgb 0 0 0)
          , Eff.display background
          , patterns ]