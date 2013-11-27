module Cube where

{-| Generates a rotating 3d cube

@docs cube
-}

import Effect(Effect, effect)

import Common(vector, transform3D,
              rotateX, rotateY, rotateZ,
              applyTransform3D, Face, faceBr,
              cubeFaces, transformFaces)

type State = {time:Float, faceEffects:[Effect], wireCol:Color}

cube : State -> Effect
cube s = Effect {step = step s, display = display s, name = "Cube"}

make : [Effect] -> Color -> Effect
make effects wireCol = cube {time=0, faceEffects=effects, wireCol=wireCol}

step : State -> Float -> Effect
step ({time} as state) delta = cube { state | time <- time + delta }


calcFaces : Float -> [Face]
calcFaces time =
  let
    rx = rotateX (0.00053*time)
    ry = rotateY (0.00087*time)
    rz = rotateZ (0.00114*time)
  in
    cubeFaces |> transformFaces rx |> transformFaces ry |> transformFaces rz

displayFace : Color -> Face -> Form -> Form
displayFace wireCol ({tl,tr,bl} as face) form =
  let
    br = faceBr face
    vtp {x,y} = (x,y)
    width = 3
    lsGray = solid wireCol
    grayLSWide = { lsGray | width <- width, join <- Smooth, cap <- Round }
    outline = path [vtp tr, vtp tl, vtp bl, vtp br, vtp tr]
  in
    outline |> traced grayLSWide

{-| Returns a rotating 3d cube effect filled form
depending on the current time. -}
display : State -> Form
display ({time, wireCol} as state) =
  let
    faces = calcFaces time
    dummyForm = rect 200 200 |> filled (rgb 0 255 0)
    forms = [dummyForm,dummyForm,dummyForm,dummyForm,dummyForm,dummyForm]
    facesWithForms = zip faces forms
    resultForms = map (uncurry (displayFace wireCol)) facesWithForms
    --resultForms = [(uncurry displayFace) (head facesWithForms)]
  in
    group resultForms |> scale 0.5