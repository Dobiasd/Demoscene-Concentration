module Tunnel where

{-| Generates a tunnel effect.

@docs tunnel
-}

-- todo: tunnel made of squares with missing elements
--       different colors, colors also go forwars but slower than we

import Effect(Effect, effect)
import Effect
import Starfield
import Common(Disc,randoms,nonOverlappingQuadruples,
              disc,vector,project2d,dist,displayDisc,
              sortBy,point,Point,displayPositionedForms)

type State = {time:Float, background:Effect, discs:[Disc]}

tunnel : State -> Effect
tunnel s = Effect {step = step s, display = display s, name = "Tunnel"}

make : Effect
make = tunnel { time=0
              , background=Starfield.make Starfield.BW 0.3 128
              , discs=[] }

minDist = 2

discInAllowedRange : Disc -> Bool
discInAllowedRange ({x,y,z} as disc) =
  let
    pos2d = project2d disc
  in
    z < (-minDist) &&
    pos2d.x >= -100 && pos2d.x <= 100 &&
    pos2d.y >= -100 && pos2d.y <= 100

calcPosOffset : Float -> Float -> Point
calcPosOffset time z =
  point (z * cos time) (z * sin time)

-- todo: now too much discs are produces
generateNewDiscRing : Float -> [Disc]
generateNewDiscRing time =
  let
    calcCol v = hsva (v*0.06) 1 1 1
    f i =
        disc (13 * cos i)
             (13 * sin i)
             -160
             (13 * cos i)
             (13 * sin i)
              0
             (calcCol i)
  in
    map toFloat [0..13] |> map ((+) time) |> map f

stepDisc : Float -> Disc -> Disc
stepDisc delta ({z} as disc) = { disc | z <- z + 0.03 * delta }

stepDiscs : Float -> [Disc] -> [Disc]
stepDiscs delta = map (stepDisc delta)

step : State -> Float -> Effect
step ({time, discs, background} as state) delta =
  let
    oldDiscs = discs |> (stepDiscs delta) |> filter discInAllowedRange
    newAmount = max 0 (10 - length oldDiscs)
    newDiscs = if (isEmpty oldDiscs ) || (last oldDiscs).z > -150 then generateNewDiscRing time else []
    discs' = oldDiscs ++ newDiscs
  in
    tunnel { state | time <- time + delta
                   , background <- Effect.step background delta
                   , discs <- discs' }



{-| Returns a tunnel effect filled form depending on the current time. -}
display : State -> Form
display ({time,background,discs} as state) =
  let
    zBufCmp = (\a b -> a.z > b.z)
    discsForm = map displayDisc discs |> displayPositionedForms
    moveForm {x,y,f} = f |> move (x,y)
  in
    [ Effect.display background
    , discsForm ]
    |> group

  --group [ rect 200 200 |> filled (rgb 0 255 255) ]