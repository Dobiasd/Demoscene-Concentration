module Cube where

{-| Generates a rotating 3d cube

@docs cube
-}

import Effect(Effect, effect)

import Common(vector, transform3D, rotateX, applyTransform3D)

import Quaternion

type State = {time:Float, faces:[Effect]}

cube : State -> Effect
cube s = Effect {step = step s, display = display s, name = "Particles"}

make : [Effect] -> Effect
make effects = cube {time=0, faces=effects}

step : State -> Float -> Effect
step ({time} as state) delta = cube { state | time <- time + delta }



--calcPositions : Float ->

-- todo: isometric projection?

{-| Returns a rotating 3d cube effect filled form
depending on the current time. -}
display : State -> Form
display ({time} as state) =
  let
    --p = (1,1,1)
    --rotation = Quaternion.angleAxis (time/10) (1,0,0)
    --p' = (Quaternion.mul (Quaternion.fromVec p) rotation)
    p = vector 1 1 1
    r = rotateX (time/500)
    p' = applyTransform3D p r
  in
  group [
    rect 200 200 |> filled (rgb 0 255 0)
--    , asText time |> toForm |> scale 0.3
  , asText p' |> toForm |> scale 0.3
  ]