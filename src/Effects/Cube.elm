module Effects.Cube where

{-| Generates a rotating 3d cube with an effect on every face.
-}

import Maybe
import List

import Effects.Effect as Eff
import Common.Vector(vector,Vector,transform3D,
              rotateX,rotateY,rotateZ,addVec,Transform3D,
              applyTransform3D,getAffineTransformation,
              subVec,crossProduct)
import Transform2D(Transform2D,matrix)

speedX = 0.00053
speedY = 0.00087
speedZ = 0.00114
borderWidth = 8

type State = {time:Float, faceEffects:[Eff.Effect], wireCol:Color}

cube : State -> Eff.Effect
cube s = Eff.Effect {step = step s, display = display s, name = "Cube"}

make : [Eff.Effect] -> Color -> Eff.Effect
make effects wireCol = cube {time=0, faceEffects=effects, wireCol=wireCol}

step : State -> Float -> Eff.Effect
step ({time,faceEffects} as state) delta =
  cube { state | time <- time + delta
               , faceEffects <-  map (\e -> Eff.step e delta) faceEffects }

type Face = { tl:Vector, tr:Vector, bl:Vector }

face : Vector -> Vector -> Vector -> Face
face tl tr bl = { tl=tl, tr=tr, bl=bl }

{-| Calculate the bottom right position of a square face. -}
faceBr : Face -> Vector
faceBr {tl,tr,bl} = (tr `subVec` tl) `addVec` bl

{-| Default cube with every face ordered as
top-left, top-right, bottom-left.
This way a face has a front and a back side and can thus be culled
if the latter if pointing towards the camera. -}
cubeFaces = [ Face (Vector (-100) ( 100) ( 100))
                   (Vector ( 100) ( 100) ( 100))
                   (Vector (-100) (-100) ( 100))
            , Face (Vector ( 100) ( 100) (-100))
                   (Vector (-100) ( 100) (-100))
                   (Vector ( 100) (-100) (-100))
            , Face (Vector ( 100) ( 100) ( 100))
                   (Vector ( 100) ( 100) (-100))
                   (Vector ( 100) (-100) ( 100))
            , Face (Vector (-100) ( 100) (-100))
                   (Vector (-100) ( 100) ( 100))
                   (Vector (-100) (-100) (-100))
            , Face (Vector (-100) ( 100) (-100))
                   (Vector ( 100) ( 100) (-100))
                   (Vector (-100) ( 100) ( 100))
            , Face (Vector (-100) (-100) ( 100))
                   (Vector ( 100) (-100) ( 100))
                   (Vector (-100) (-100) (-100))
            ]

transformFace : Transform3D -> Face -> Face
transformFace matrix {tl,tr,bl} =
  let f = applyTransform3D matrix
  in face (f tl) (f tr) (f bl)

transformFaces : Transform3D -> [Face] -> [Face]
transformFaces matrix = map (transformFace matrix)

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

renderFace : Color -> Face -> Form -> Form
renderFace wireCol ({tl,tr,bl} as face) form =
  let
    br = faceBr face
    vtp {x,y} = (x,y)
    lsjustCol = solid wireCol
    lSWide = {lsjustCol | width <- borderWidth, join <- Smooth, cap <- Round}
    outline = path [vtp tr, vtp tl, vtp bl, vtp br, vtp tr]
    (m2d,m3d) = getAffineTransformation
          (-100,100) (100,100) (-100,-100)
          (tl.x,tl.y) (tr.x,tr.y) (bl.x,bl.y)
    transformedForm = groupTransform m2d [form]
  in
    group [ transformedForm, outline |> traced lSWide ]

displayFace : Color -> Face -> Form -> Maybe Form
displayFace wireCol face form =
  if faceShowsBackside face then Nothing
                            else Just <| renderFace wireCol face form

display : State -> Form
display ({time, wireCol, faceEffects} as state) =
  let
    faces = calcFaces (time/2)
    forms = map Eff.display faceEffects
    facesWithForms = zip faces forms
    resultForms = List.filterMap identity
      <| map (uncurry (displayFace wireCol)) facesWithForms
  in
    group resultForms |> scale (1/sqrt(3))