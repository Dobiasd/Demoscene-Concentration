module Moire where

{-| Generates a moire effect.

@docs moire
-}

import Effect(Effect,effect)
import Effect
import Common(Vector,vector,Transform3D,applyTransform3D,rotateZ)
import Starfield

type State = {time:Float, background:Effect}

moire : State -> Effect
moire s = Effect {step = step s, display = display s, name = "Moire"}

make : Effect
make = moire {time=0, background=Starfield.make Starfield.Colored 1.1 128}

step : State -> Float -> Effect
step ({time, background} as state) delta =
  moire { state | time <- time + delta
                , background <- Effect.step background delta }

type Line = (Vector, Vector)

line : Vector -> Vector -> Line
line s e = (s, e)

pattern : [Line]
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
    --lS1 = solid (rgba 127 127 255 0.1)
    lS1 = solid (hsva (2.6 + cos num) 1 1 0.1)
    lS1Wide = { lS1 | width <- width, join <- Smooth, cap <- Round }
    outline = path [(s.x,s.y), (e.x,e.y)]
  in
    outline |> traced lS1Wide


displayLines : [Line] -> Float -> Form
displayLines pattern angle =
  let
    r = rotateZ angle
    lines = map (transformLine r) pattern
    nums = map toFloat [0..(length(lines))]
  in
    map (uncurry displayLine) (zip nums lines) |> group

{-| Returns a moire effect filled form depending on the current time. -}
display : State -> Form
display ({time, background} as state) =
  let
    angles = map ((*)(time/20000)) [1..15]
    patterns = map (displayLines pattern) angles
  in
    [
      rect 200 200 |> filled (rgb 0 0 0)
    , Effect.display background
    ] ++ patterns
    |> group