module Effects.Plasma where

{-| Generates a plasma effect.

@docs plasma
-}

import Effects.Effect(Effect, effect)
import Common.Algorithms(uncurry4, nonOverlappingQuadruples)
import Common.Random(randomFloats)

pixels = 8
speed = 0.0004

type State = {time:Float}

plasma : State -> Effect
plasma s = Effect {step = step s, display = display s, name = "Plasma"}

make : Effect
make = plasma {time=0}

step : State -> Float -> Effect
step ({time} as state) delta = plasma { state | time <- time + delta }

{-| Returns a plasma effect filled form depending on the current time. -}
display : State -> Form
display ({time} as state) =
  let
    poss = rectPositions pixels pixels
    colR (x,y) = pseudoBilinearInterpolatedRect (x,y+1) (x+1,y)
      (plasmaCol (x)   (y+1) time)
      (plasmaCol (x)   (y)   time)
      (plasmaCol (x+1) (y)   time)
      (plasmaCol (x+1) (y+1) time)
    rectForms = map colR poss |> map (move (-pixels/2,-pixels/2))
  in
    [rect pixels pixels |> filled black] ++ rectForms
      |> group |> scale (200/pixels)


{-| Returns all integral positions inside a rect.
rectPositions 2 3 =
  [(0,0),(1,0),(0,1),(1,1),(0,2),(1,2)]
-}
rectPositions : Float -> Float -> [(Float,Float)]
rectPositions w h =
  let
    xs = [0..(w-1)]
    ys = [0..(h-1)]
    row y = map (flip (,) <| y) xs
  in
    map row ys |> concat


type PlasmaColConf = { sf:Float, xf:Float, yf:Float, tf:Float }


plasmaColConf : Float -> Float -> Float -> Float -> PlasmaColConf
plasmaColConf sf xf yf tf = { sf=sf, xf=xf, yf=yf, tf=tf }


genPlasmaColConfs : Float -> Int -> [ PlasmaColConf ]
genPlasmaColConfs seed amount =
  let
    randoms = randomFloats seed (amount*4) |> map (\x -> 16 * x - 8)
  in
    map (uncurry4 plasmaColConf) (nonOverlappingQuadruples randoms)

rConf = genPlasmaColConfs 1 8
gConf = genPlasmaColConfs 2 8
bConf = genPlasmaColConfs 3 8

colValFromConf : Float -> Float -> Float -> PlasmaColConf -> Float
colValFromConf x y t conf =
  conf.sf * cos ( (conf.xf * x) + (conf.yf * y) + (conf.tf * t) )

divisorForColConfs : [PlasmaColConf] -> Float
divisorForColConfs confs =
  sum <| map (abs . .sf) confs

colValFromConfs : Float -> Float -> Float -> [PlasmaColConf] -> Float
colValFromConfs x y t confs =
  (sum <| map (colValFromConf x y t) confs) / divisorForColConfs confs

calcPixelCol : Float -> Float -> Float -> [PlasmaColConf] -> Int
calcPixelCol x y t confs =
  let
    clampCol = clamp 0 255
    center = 32
    factor = 224
    rawCol = colValFromConfs x y t confs
  in
    round (center + factor * rawCol) |> clampCol

plasmaCol : Float -> Float -> Float -> Color
plasmaCol xIn yIn tIn =
  let
    colFunc = calcPixelCol (xIn / pixels) (yIn / pixels) (tIn * speed)
  in
    rgb (colFunc rConf) (colFunc gConf) (colFunc bConf)

pseudoBilinearInterpolatedRect : (Float,Float) -> (Float,Float) ->
  Color -> Color -> Color -> Color -> Form
pseudoBilinearInterpolatedRect
    ((tlx,tly) as tl)
    ((brx,bry) as br)
    ((Color rtl gtl btl atl) as ctl)
    ((Color rbl gbl bbl abl) as cbl)
    ((Color rbr gbr bbr abr) as cbr)
    ((Color rtr gtr btr atr) as ctr) =
  let
    ((trx,try) as tr) = (brx,tly)
    ((blx,bly) as bl) = (tlx,bry)
    g1cm = rgba
      ((rtl+rbr) `div` 2) ((gtl+gbr) `div` 2) ((btl+bbr) `div` 2) 0.7
    g2cm = rgba
      ((rtr+rbl) `div` 2) ((gtr+gbl) `div` 2) ((btr+bbl) `div` 2) 0.03
    (w,h) = (brx-tlx, tly-bry)
    ((cx,cy) as c) = (tlx + w/2, tly - h/2)
    tlg = (tlx - cx, tly - cy)
    blg = (blx - cx, bly - cy)
    brg = (brx - cx, bry - cy)
    trg = (trx - cx, try - cy)
    g1 = linear tlg brg [(0,ctl), (0.5,g1cm), (1,cbr)]
    g2 = linear trg blg [(0,ctr), (0.5,g2cm), (1,cbl)]
  in
    group [
      gradient g1 (rect w h)
    , gradient g2 (rect w h)
    ] |> move c