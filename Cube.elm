module Cube where

{-| Generates a rotating 3d cube

@docs cube
-}

import Effect(Effect, effect)

import Common(vector, transform3D,
              rotateX, rotateY, rotateZ,
              applyTransform3D, Face, faceBr,
              cubeFaces, transformFaces)

--import Quaternion

type State = {time:Float, faces:[Effect]}

cube : State -> Effect
cube s = Effect {step = step s, display = display s, name = "Particles"}

make : [Effect] -> Effect
make effects = cube {time=0, faces=effects}

step : State -> Float -> Effect
step ({time} as state) delta = cube { state | time <- time + delta }


calcFaces : Float -> [Face]
calcFaces time =
  let
    rx = rotateX (0.0011*time)
    ry = rotateY (0.0013*time)
    rz = rotateZ (0.0015*time)
  in
    cubeFaces |> transformFaces rx |> transformFaces ry |> transformFaces rz

displayFace : Face -> Form -> Form
displayFace ({tl,tr,bl} as face) form =
  let
    br = faceBr face
    vtp {x,y} = (x,y)
    width = 3
    lsGray = solid (rgba 0 0 0 0.3)
    grayLSWide = { lsGray | width <- width, join <- Smooth, cap <- Round }
    outline = path [vtp tr, vtp tl, vtp bl, vtp br, vtp tr]
  in
    outline |> traced grayLSWide

{-| Returns a rotating 3d cube effect filled form
depending on the current time. -}
display : State -> Form
display ({time} as state) =
  let
    faces = calcFaces time
    dummyForm = rect 200 200 |> filled (rgb 0 255 0)
    forms = [dummyForm,dummyForm,dummyForm,dummyForm,dummyForm,dummyForm]
    facesWithForms = zip faces forms
    resultForms = map (uncurry displayFace) facesWithForms
    --resultForms = [(uncurry displayFace) (head facesWithForms)]
  in
    group resultForms |> scale 0.5