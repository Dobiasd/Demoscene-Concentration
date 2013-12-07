module Effects.Moire where

{-| Generates a moire effect.

@docs moire
-}

import Effects.Effect(Effect,effect)
import Effects.Effect as Effect
import Common.Vector(Vector,vector,Transform3D,applyTransform3D,rotateZ)
import Effects.Starfield as Starfield

type State = {time:Float, background:Effect}

moire : State -> Effect
moire s = Effect {step = step s, display = display s, name = "Moire"}

make : Effect
make = moire {time=0, background=Starfield.make Starfield.Colored 0.2 64}

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
    lS = solid (hsva (2.6 + cos num) 1 1 0.1)
    --lSWide = { lS | width <- width, join <- Smooth, cap <- Round }
    lSWide = { lS | width <- width }
    outline = path [(s.x,s.y), (e.x,e.y)]
  in
    outline |> traced lSWide


displayLines : [Line] -> Float -> Form
displayLines pattern angle =
  let
    r = rotateZ angle
    lines = map (transformLine r) pattern
    nums = map toFloat [0..(length(lines))]
  in
    map (uncurry displayLine) (zip nums lines) |> group


display : State -> Form
display ({time, background} as state) =
  let
    angles = map ((*)(time/20000)) [1..15]
    patterns = map (displayLines pattern) angles |> group
  in
    group [ rect 200 200 |> filled (rgb 0 0 0)
          , Effect.display background
          , patterns ]