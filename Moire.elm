module Moire where

{-| Generates a moire effect.

@docs moire
-}

import Effect(Effect, effect)
import Common(Vector,vector,Transform3D,applyTransform3D,rotateZ)

type State = {time:Float}

moire : State -> Effect
moire s = Effect {step = step s, display = display s, name = "Moire"}

make : Effect
make = moire {time=0}

step : State -> Float -> Effect
step ({time} as state) delta = moire { state | time <- time + delta }

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

displayLine : Line -> Form
displayLine (s, e) =
  let
    width = 8
    lS1 = solid (rgba 127 127 255 0.1)
    lS1Wide = { lS1 | width <- width, join <- Smooth, cap <- Round }
    outline = path [(s.x,s.y), (e.x,e.y)]
  in
    outline |> traced lS1Wide


displayLines : [Line] -> Float -> Form
displayLines pattern angle =
  let
    r = rotateZ angle
    lines = map (transformLine r) pattern
  in
    map displayLine lines |> group

{-| Returns a moire effect filled form depending on the current time. -}
display : State -> Form
display ({time} as state) =
  let
    angles = map ((*)(time/20000)) [1..15]
    patterns = map (displayLines pattern) angles
  in
    [
      rect 200 200 |> filled (rgb 0 0 0)
    --, asText time |> toForm
    ] ++ patterns
    |> group