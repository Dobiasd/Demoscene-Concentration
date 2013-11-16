module Plasma where

{-| Generates a plasma effect.

@docs plasma
-}


type State = {time:Float}

make : State
make = {time=0}

step : Float -> State -> State
step delta ({time} as state) = { state | time <- time + delta }

{-| Returns a plasma effect filled form depending on the current time. -}
display : State -> Form
display ({time} as state) =
  let
    t = time -- todo: Why you no work with time directly?
    pixels = 8
    poss = rectPositions pixels pixels
    colR (x,y) = bilinearInterpolatedRect (x,y+1) (x+1,y)
      (plasmaCol (x)   (y+1) t)
      (plasmaCol (x)   (y)   t)
      (plasmaCol (x+1) (y)   t)
      (plasmaCol (x+1) (y+1) t)
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


rConf : [ PlasmaColConf ]
rConf = [ plasmaColConf -4   3  -4   1
        , plasmaColConf  6   6   1   5
        , plasmaColConf  2   5  -7  -8
        , plasmaColConf -3   5   3  -7
        , plasmaColConf  5   1  -7   2
        , plasmaColConf -4   8  -1  -3
        , plasmaColConf -5   7   4   7
        , plasmaColConf  3   8  -5   1
        ]

gConf : [ PlasmaColConf ]
gConf = [ plasmaColConf  4  -2  -3  -2
        , plasmaColConf  2   1  -2   5
        , plasmaColConf -6   2  -6  -6
        , plasmaColConf -4   6   6  -3
        , plasmaColConf -6   6  -5  -3
        , plasmaColConf  4  -8   4  -5
        , plasmaColConf  1   3   6   1
        , plasmaColConf  5   6  -2  -5
        ]

bConf : [ PlasmaColConf ]
bConf = [ plasmaColConf  6  -7   6   1
        , plasmaColConf -7  -2  -8   7
        , plasmaColConf  2   7  -1   8
        , plasmaColConf -3  -4   4  -1
        , plasmaColConf  3  -4   8   7
        , plasmaColConf -4   8   0   6
        , plasmaColConf  4  -1   5  -3
        , plasmaColConf  1   5  -4  -3
        ]

colValFromConf : Float -> Float -> Float -> PlasmaColConf -> Float
colValFromConf x y t conf =
  conf.sf * cos ( (conf.xf * x) + (conf.yf * y) + (conf.tf * t) )

divisorForColConfs : [PlasmaColConf] -> Float
divisorForColConfs confs =
  sum <| map (abs . .sf) confs

colValFromConfs : Float -> Float -> Float -> [PlasmaColConf] -> Float
colValFromConfs x y t confs =
  (sum <| map (colValFromConf x y t) confs) / divisorForColConfs confs

plasmaCol : Float -> Float -> Float -> Color
plasmaCol xIn yIn tIn =
  let
    x = xIn / 7
    y = yIn / 7
    t = tIn * 0.0004
    rRaw = colValFromConfs x y t rConf
    gRaw = colValFromConfs x y t gConf
    bRaw = colValFromConfs x y t bConf
    correctCol = clamp 0 255
    center = 32
    factor = 224
  in
    rgb
      (round (center + factor * rRaw) |> correctCol)
      (round (center + factor * gRaw) |> correctCol)
      (round (center + factor * bRaw) |> correctCol)


bilinearInterpolatedRect : (Float,Float) -> (Float,Float) ->
  Color -> Color -> Color -> Color -> Form
bilinearInterpolatedRect
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