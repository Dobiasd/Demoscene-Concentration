module Effects.Tunnel where

{-| Generates a tunnel effect.

@docs tunnel
-}

import Effects.Effect(Effect, effect)
import Effects.Effect as Effect
import Effects.Starfield as Starfield
import Common.Vector(vector,project2d,dist,point2D,normalize,scalarProd)
import Common.Algorithms(sortBy)
import Common.Display(displayPositionedForms,
                      PositionedForm, isPosOK, positionedForm)
import Common.Types(WithRadius,WithNormal,Colored,Positioned,Point,point2D)

type State = {time:Float, background:Effect, discs:[Disc]}

tunnel : State -> Effect
tunnel s = Effect {step = step s, display = display s, name = "Tunnel"}

make : Effect
make = tunnel { time=0
              , background=Starfield.make Starfield.BW 0.05 64
              , discs=[] }

minDist = 2

type Disc = WithRadius ( Colored ( Positioned ( WithNormal {} ) ) )

disc : Float -> Float -> Float -> Float -> Float -> Float -> Color -> Disc
disc x y z nx ny nz c = {x=x, y=y, z=z, col=c, r=400, nx=nx,ny=ny,nz=nz}

displayDisc : Disc -> PositionedForm
displayDisc ({x,y,z,nx,ny,nz,col,r} as disc) =
  let
    c2d = project2d disc
    r2d = r / (-z)
    normToCam = normalize disc
    normale = normalize {x=nx,y=ny,z=nz}
    proj = normale `scalarProd` normToCam
    discHeight = proj * r2d
    angle = atan2 normale.x normale.y
    centeredForm = oval r2d discHeight |> filled col |> rotate -angle
  in
    positionedForm centeredForm {x=c2d.x,y=c2d.y,z=z}

discInAllowedRange : Disc -> Bool
discInAllowedRange ({x,y,z} as disc) =
  isPosOK (project2d disc) && z < (-minDist)

calcPosOffset : Float -> Float -> Point
calcPosOffset time z =
  point2D (z * cos time) (z * sin time)

generateNewDiscRing : Float -> [Disc]
generateNewDiscRing time =
  let
    calcCol v = hsva (v*0.06) 1 1 1
    n = 11
    generateDisc i =
      let
        r = 12
        x = r * cos (i+time)
        y = r * sin (i+time)
        z = -110
        off = 15
        xOff = off * (cos (0.00104*time) + cos (0.00056*time))
        yOff = off * (cos (0.00045*time) + cos (0.00087*time))
      in
        disc (x+xOff) (y+yOff) z x y 0 (calcCol (i+time))
  in
    map toFloat [0..n] |> map ((+) time) |> map generateDisc

stepDisc : Float -> Disc -> Disc
stepDisc delta ({z} as disc) = { disc | z <- z + 0.05 * delta }

stepDiscs : Float -> [Disc] -> [Disc]
stepDiscs delta = map (stepDisc delta)

step : State -> Float -> Effect
step ({time, discs, background} as state) delta =
  let
    oldDiscs = discs |> (stepDiscs delta) |> filter discInAllowedRange
    newAmount = max 0 (10 - length oldDiscs)
    newDiscs = if (isEmpty oldDiscs ) || (last oldDiscs).z > -105 then generateNewDiscRing time else []
    discs' = oldDiscs ++ newDiscs
  in
    tunnel { state | time <- time + delta
                   , background <- Effect.step background delta
                   , discs <- discs' }

display : State -> Form
display ({time,background,discs} as state) =
  let
    zBufCmp = (\a b -> a.z > b.z)
    discsForm = map displayDisc discs |> displayPositionedForms
    moveForm {x,y,f} = f |> move (x,y)
  in
    group [ Effect.display background
          , discsForm ]