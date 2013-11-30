module Particles where

{-| Generates a particle effect.

@docs particles
-}

import Effect(Effect, effect)
import Common(Vector,vector,Transform3D,applyTransform3D,rotateY,project2d,
              randoms,nonOverlappingSextuples,
              WithRadius,Positioned3,Moving3,Colored,
              decomposeColor,addVec,move3,distTo,multVec,subVec,
              PositionedForm, positionedForm, displayPositionedForms, isPosOK)

type Ball = Positioned3 (Moving3 (Colored {}))

type State = {time:Float,balls:[Ball]}

ball : Float -> Float -> Float
    -> Float -> Float -> Float
    -> Color -> Ball
ball x y z vx vy vz col = {x=x,y=y,z=z,vx=vx,vy=vy,vz=vz,col=col}

make : Effect
make = particles {time=0, balls=[]}

generateNewBalls : Int -> Float -> [Ball]
generateNewBalls amount time =
  let
    randomFloats = randoms time (amount*6)
    sextuples = nonOverlappingSextuples randomFloats
    f (x,y,z,vx,vy,vz) =
      ball (40 * x - 20) (40 * y - 20) (40 * z - 20)
           (0.2 * vx - 0.1) (0.2 * vy - 0.1) (0.2 * vz - 0.1)
           (hsv (2*x*y) 1 1)
  in
    map f sextuples

displayBallShadow : Ball -> PositionedForm
displayBallShadow ({x,y,z} as b) =
  let
    radius = 100 / (-z)
    sOff = 0.03*y * abs (y - origin.y)
    sharpness = clamp 0.1 0.3 (0.3 - abs (y - origin.y) / 60)
    pos2d = project2d {x=x+sOff,y=origin.y,z=z-sOff/2}
    grad = radial (0,0) 0 (0,0) radius
          [(0, rgba 0 0 0 (0.1+2*sharpness)),
           (1, rgba 0 0 0 0)]
  in
    positionedForm (oval radius (0.3*radius) |> gradient grad) {x=pos2d.x,y=pos2d.y-(0.7*radius),z=z}

displayBall : Ball -> PositionedForm
displayBall ({x,y,z,col} as ball) =
  let
    (r,g,b,a) = decomposeColor col
    radius = 100 / (-z)
    grad = radial (0,0) 0 (0,0) radius
          [(0  , rgba r g b 0.9),
           (0.3, rgba r g b 0.7),
           (1  , rgba r g b 0.2)]
    pos2d = project2d ball
  in
    positionedForm (circle radius |> gradient grad) {x=pos2d.x,y=pos2d.y,z=z}


displayBallWithShadow : Ball -> [PositionedForm]
displayBallWithShadow b = [ displayBallShadow b, displayBall b]

stepBallUsual : Float -> Ball -> Ball
stepBallUsual delta ({x,y,z,vx,vy,vz,col} as b) =
  let
    p' = vector (x + vx)
                (max (origin.y) (y + vy))
                (z + vz)
    vy' = if y <= origin.y then abs (0.6 * vy)
                      else (vy - 0.001 * delta)
    v' = vector vx vy' vz
  in
    { b | x <- p'.x, y <- p'.y, z <- p'.z
       , vx <- v'.x, vy <- v'.y, vz <- v'.z }

throwBall : Ball -> Ball
throwBall ({vy} as b) = { b | vy <- 1.8 }

pullToRoot : Float -> Ball -> Ball
pullToRoot delta ({x,y,z,vx,vy,vz} as b) =
  let
    diff = origin `subVec` b
    pull = diff `multVec` (0.00001 * delta)
  in
    { b | vx <- vx + pull.x
        , vy <- vy + pull.y
        , vz <- vz + pull.z }

origin = {x=0,y=-15,z=0}

stepBall : Float -> Ball -> Ball
stepBall delta b =
  let
    throw = distTo b origin < 3.0
    stepF = (stepBallUsual delta) . (pullToRoot delta)
  in
    if throw then throwBall b |> stepF else stepF b


transformBall : Transform3D -> Ball -> Ball
transformBall m b = applyTransform3D m b

particles : State -> Effect
particles s = Effect {step = step s, display = display s, name = "Particles"}

step : State -> Float -> Effect
step ({time, balls} as state) delta =
  let
    oldBalls = balls |> map (stepBall delta)
    newAmount = max 0 (92 - length oldBalls)
    balls' = oldBalls ++ generateNewBalls newAmount time
  in
    particles { state | time <- time + delta
                      , balls <- balls' }

displayFloor : Form
displayFloor =
  let
    grad = linear (0,40) (0,-40)
         [(0, rgb 0  41  67),
          (1, rgb 0 171 235)]
    floorForm = rect 200 80 |> gradient grad |> move (0,-60)
  in
    floorForm

displayStar : Vector -> PositionedForm
displayStar star =
  let
    {x,y} = project2d star
  in
    positionedForm (circle 0.6 |> filled white |> move (x,y)) {x=x,y=y,z=star.z}

staticStars : [Vector]
staticStars =
  let
    f i = vector (     100 * (cos (1234 * i)))
                 ( 30 + 30 * (cos (2345 * i)))
                 (     100 * (cos (3456 * i)))
  in
    map f [0..128]


{-| Returns a particle effect filled form depending on the current time. -}
display : State -> Form
display ({time,balls} as state) =
  let
    m = rotateY (0.0003*time)
    rotatedBalls = map (transformBall m) balls
    starForms = map (applyTransform3D m) staticStars |> map displayStar |> filter isPosOK |> map .f
    m2 = move3 (vector 0 0 (-30))
    moved2Balls = map (transformBall m2) rotatedBalls
    ballForms = map displayBallWithShadow moved2Balls |> concat |> displayPositionedForms
  in
    [
      rect 200 200 |> filled (rgb 0 0 0)
    ] ++ starForms ++ displayFloor :: [ballForms]
    |> group