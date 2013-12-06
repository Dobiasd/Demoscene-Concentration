module Cube where

{-| Generates a rotating 3d cube

@docs cube
-}

import Effect(Effect, effect)
import Effect

import Common(vector, transform3D,
              rotateX, rotateY, rotateZ,
              applyTransform3D, Face, faceBr,
              cubeFaces, transformFaces, getAffineTransformation,
              subVec, crossProduct, dummyForm)

import Transform2D(Transform2D,matrix)

speedX = 0.00053
speedY = 0.00087
speedZ = 0.00114
borderWidth = 8

type State = {time:Float, faceEffects:[Effect], wireCol:Color}

cube : State -> Effect
cube s = Effect {step = step s, display = display s, name = "Cube"}

make : [Effect] -> Color -> Effect
make effects wireCol = cube {time=0, faceEffects=effects, wireCol=wireCol}

step : State -> Float -> Effect
step ({time,faceEffects} as state) delta =
  cube { state | time <- time + delta
               , faceEffects <-  map (\e -> Effect.step e delta) faceEffects }

calcFaces : Float -> [Face]
calcFaces time =
  let
    rx = rotateX (speedX*time)
    ry = rotateY (speedY*time)
    rz = rotateZ (speedZ*time)
  in
    cubeFaces |> transformFaces rx |> transformFaces ry |> transformFaces rz

faceShowsBackside : Face -> Bool
faceShowsBackside {tl,tr,bl} =
  let
    vRight = tr `subVec` tl
    vDown = bl `subVec` tl
    normale = vDown `crossProduct` vRight
  in
    normale.z < 0

displayFace : Color -> Face -> Form -> Form
displayFace wireCol ({tl,tr,bl} as face) form =
  let
    br = faceBr face
    vtp {x,y} = (x,y)
    lsjustCol = solid wireCol
    lSWide = { lsjustCol | width <- borderWidth, join <- Smooth, cap <- Round }
    outline = path [vtp tr, vtp tl, vtp bl, vtp br, vtp tr]
    (m2d,m3d) = getAffineTransformation
          (-100,100) (100,100) (-100,-100)
          (tl.x,tl.y) (tr.x,tr.y) (bl.x,bl.y)
    transformedForm = groupTransform m2d [form]
  in
    if faceShowsBackside face then dummyForm else
      group [ transformedForm, outline |> traced lSWide ]

{-| Returns a rotating 3d cube effect filled form
depending on the current time. -}
display : State -> Form
display ({time, wireCol, faceEffects} as state) =
  let
    faces = calcFaces (time/2)
    forms = map Effect.display faceEffects
    facesWithForms = zip faces forms
    resultForms = map (uncurry (displayFace wireCol)) facesWithForms
  in
    group resultForms |> scale (1/sqrt(3))