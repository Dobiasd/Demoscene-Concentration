module Effects.Particles where

{-| Generates a particle effect.
-}

import Effects.Effect(Effect, effect)
import Common.Random(randomFloats)
import Common.Algorithms(nonOverlappingTriples,nonOverlappingPairs)
import Common.Types(WithRadius,Positioned,Moving,Colored)
import Common.Display(PositionedForm,positionedForm,
                      displayPositionedForms,isPosOK,decomposeColor)
import Common.Vector(Vector,vector,Transform3D,applyTransform3D,
                     rotateY,project2d,addVec,move3,
                     distTo,multVec,subVec)

type Ball = Positioned (Moving (Colored {}))

type State = {time:Float,balls:[Ball]}

ball : Float -> Float -> Float
    -> Float -> Float -> Float
    -> Color -> Ball
ball x y z vx vy vz col = {x=x,y=y,z=z,vx=vx,vy=vy,vz=vz,col=col}

make : Effect
make = particles {time=0, balls=generateBalls 64 1.23}

generateBalls : Int -> Float -> [Ball]
generateBalls amount time =
  let
    randoms = randomFloats time (amount*6)
    sextuples = randoms |> nonOverlappingTriples |> nonOverlappingPairs
    f ((x,y,z),(vx,vy,vz)) =
      ball (40 * x - 20) (40 * y - 20) (40 * z - 20)
           (0.2 * vx - 0.1) (0.2 * vy - 0.1) (0.2 * vz - 0.1)
           (hsv (2*x*y) 1 1)
  in
    map f sextuples

displayBallShadow : Float -> Ball -> PositionedForm
displayBallShadow angle ({x,y,z} as b) =
  let
    radius = 100 / (-z)
    sOff = let distToFloor = abs (y - origin.y) in 0.4 * distToFloor
    xOff = sOff * sin angle
    yOff = sOff * cos angle
    sharpness = clamp 0.1 0.3 (0.3 - abs (y - origin.y) / 60)
    pos2d = project2d {x=x+xOff,y=origin.y,z=z+yOff}
    grad = radial (0,0) 0 (0,0) radius
                  [ (0, rgba 0 0 0 (0.1+2*sharpness))
                  , (1, rgba 0 0 0 0) ]
  in
    positionedForm (oval radius (0.3*radius) |> gradient grad)
                   {x=pos2d.x,y=pos2d.y-(0.7*radius),z=z}

displayBall : Ball -> PositionedForm
displayBall ({x,y,z,col} as ball) =
  let
    (r,g,b,a) = decomposeColor col
    radius = 100 / (-z)
    grad = radial (0,0) 0 (0,0) radius
                  [ (0  , rgba r g b 0.9)
                  , (0.3, rgba r g b 0.7)
                  , (1  , rgba r g b 0.2) ]
    pos2d = project2d ball
  in
    positionedForm (circle radius |> gradient grad)
                   {x=pos2d.x,y=pos2d.y,z=z}

displayBallWithShadow : Float -> Ball -> [PositionedForm]
displayBallWithShadow shadowAngle b =
  [ displayBallShadow shadowAngle b, displayBall b]

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
    { b | x  <- p'.x,  y <- p'.y,  z <- p'.z
        , vx <- v'.x, vy <- v'.y, vz <- v'.z }

throwBall : Ball -> Ball
throwBall ({vy} as b) = { b | vy <- 2.0 }

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
    balls' = balls |> map (stepBall delta)
  in
    particles { state | time <- time + delta
                      , balls <- balls' }

displayFloor : Form
displayFloor =
  let
    grad = linear (0,40) (0,-40)
         [ (0, rgb 0  41  67)
         , (1, rgb 0 171 235) ]
    floorForm = rect 200 80 |> gradient grad |> move (0,-60)
  in
    floorForm

displayStar : Vector -> PositionedForm
displayStar star =
  let
    {x,y} = project2d star
  in
    positionedForm (circle 0.6 |> filled white |> move (x,y))
                   {x=x,y=y,z=star.z}

staticStars : [Vector]
staticStars =
  let
    f i = vector (     100 * (cos (1234 * i)))
                 ( 25 + 20 * (cos (2345 * i)))
                 (     100 * (cos (3456 * i)))
  in
    map f [0..64]

display : State -> Form
display ({time,balls} as state) =
  let
    rotateAngle = 0.0003*time
    m = rotateY rotateAngle
    rotatedBalls = map (transformBall m) balls
    starForms = map (applyTransform3D m) staticStars |> map displayStar |>
                  filter isPosOK |> map .f
    m2 = move3 (vector 0 0 (-30))
    movedBalls = map (transformBall m2) rotatedBalls
    ballForms = map (displayBallWithShadow rotateAngle) movedBalls |> concat |>
                  displayPositionedForms
  in
    [ rect 200 200 |> filled (rgb 0 0 0) ]
      ++ starForms ++ displayFloor :: [ballForms]
      |> group