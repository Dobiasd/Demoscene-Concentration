module Particles where

{-| Generates a particle effect.

@docs particles
-}

-- todo: fractal tree, glow to floor, positioned forms (z-buffer), zsort


import Effect(Effect, effect)
import Common(Vector,vector,Transform3D,applyTransform3D,rotateY,project2d,
              randoms,nonOverlappingSextuples,
              WithRadius,Positioned3,Moving3,Colored,
              decomposeColor,addVec,move3,distTo,multVec,subVec)

type State = {time:Float,balls:[Object]}

-- todo radius needed?
type BallCore = Positioned3 (Moving3 (Colored {age:Float}))

ballCore : Float -> Float -> Float
        -> Float -> Float -> Float
        -> Color -> BallCore
ballCore x y z vx vy vz col =
  {x=x,y=y,z=z,vx=vx,vy=vy,vz=vz,col=col,age=0}

data Object = Ball BallCore | Line Vector Vector Color

make : Effect
make = particles {time=0, balls=[]}

generateNewBalls : Int -> Float -> [Object]
generateNewBalls amount time =
  let
    randomFloats = randoms time (amount*6)
    sextuples = nonOverlappingSextuples randomFloats
    f (x,y,z,vx,vy,vz) = Ball <|
      ballCore (40 * x - 20) (40 * y - 20) (40 * z - 20)
               (0.2 * vx - 0.1) (0.2 * vy - 0.1) (0.2 * vz - 0.1)
               (rgba 255 192 128 0.4)
  in
    map f sextuples

displayBallShadow : BallCore -> Form
displayBallShadow ({x,y,z} as bc) =
  let
    radius = 100 / (-z)
    sOff = 0.03*y * abs (y - origin.y)
    sharpness = clamp 0.1 0.3 (0.3 - abs (y - origin.y) / 60)
    pos2d = project2d {x=x+sOff,y=origin.y,z=z-sOff/2}
    grad = radial (0,0) 0 (0,0) radius
          [(0, rgba 0 0 0 (0.1+2*sharpness)),
           (1, rgba 0 0 0 0)]
  in
    if z < -1 && pos2d.x >= -100 && pos2d.x <= 100 then
      oval radius (0.3*radius) |> gradient grad |> move (pos2d.x,pos2d.y-(0.7*radius))
      else rect 0 0 |> filled (rgb 0 0 0)

displayBall : BallCore -> Form
displayBall ({x,y,z,col} as bc) =
  let
    (r,g,b,a) = decomposeColor col
    radius = 100 / (-z)
    grad = radial (0,0) 0 (0,0) radius
          [(0  , rgba r g b 0.7),
           (0.3, rgba r g b 0.7),
           (1  , rgba r g b 0.2)]
    pos2d = project2d bc
  in
    if z < -1 && pos2d.x >= -100 && pos2d.x <= 100 then
      circle radius |> gradient grad |> move (pos2d.x,pos2d.y)
      else rect 0 0 |> filled (rgb 0 0 0)

displayLine : Vector -> Vector -> Color -> Form
displayLine s e col =
  rect 0 0 |> filled (rgb 0 0 0)

displayObj : Object -> Form
displayObj obj = case obj of
                   (Ball bc)      -> group [ displayBallShadow bc, displayBall bc]
                   (Line s e col) -> displayLine s e col
                   _              -> rect 0 0 |> filled (rgb 0 0 0)

stepBallUsual : Float -> BallCore -> Object
stepBallUsual delta ({x,y,z,vx,vy,vz,col,age} as bc) =
  let
    p' = vector (x + vx)
                (max (origin.y) (y + vy))
                (z + vz)
    vy' = if y <= origin.y then abs (0.6 * vy)
                      else (vy - 0.001 * delta)
    v' = vector vx vy' vz
  in
    Ball <| { bc | x <- p'.x, y <- p'.y, z <- p'.z
                 , vx <- v'.x, vy <- v'.y, vz <- v'.z
                 , age <- age + delta }

throwBall : BallCore -> BallCore
throwBall ({vy} as bc) = { bc | vy <- 1.8 }

pullToRoot : Float -> BallCore -> BallCore
pullToRoot delta ({x,y,z,vx,vy,vz} as bc) =
  let
    diff = origin `subVec` bc
    pull = diff `multVec` (0.00001 * delta)
  in
    { bc | vx <- vx + pull.x
         , vy <- vy + pull.y
         , vz <- vz + pull.z }

origin = {x=0,y=-15,z=0}

stepBall : Float -> BallCore -> Object
stepBall delta bc =
  let
    throw = distTo bc origin < 3.0
    stepF = (stepBallUsual delta) . (pullToRoot delta)
  in
    if throw then throwBall bc |> stepF else stepF bc

stepObj : Float -> Object -> Object
stepObj delta obj = case obj of
                      (Ball bc) -> stepBall delta bc
                      _         -> obj

transformObj : Transform3D -> Object -> Object
transformObj m obj = case obj of
                      (Ball bc) -> Ball (applyTransform3D m bc)
                      _         -> obj


particles : State -> Effect
particles s = Effect {step = step s, display = display s, name = "Particles"}

step : State -> Float -> Effect
step ({time, balls} as state) delta =
  let
    oldBalls = balls |> map (stepObj delta)
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

displayStar : Vector -> Form
displayStar star =
  let
    pos2d = project2d star
  in
    circle 0.6 |> filled white |> move (pos2d.x,pos2d.y)

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
    rotatedBalls = map (transformObj m) balls
    starForms = map (applyTransform3D m) staticStars |> map displayStar
    m2 = move3 (vector 0 0 (-30))
    moved2Balls = map (transformObj m2) rotatedBalls
    ballForms = map displayObj moved2Balls
  in
    [
      rect 200 200 |> filled (rgb 0 0 0)
    ] ++ starForms ++ displayFloor :: ballForms
    |> group