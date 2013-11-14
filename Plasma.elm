module Plasma where

{-| Generates a plasma effect.

@docs plasma
-}




{-| Returns a plasma effect filled form depending on the current time. -}
plasma : Time -> Form
plasma t =
  let
    pixels = 16
    poss = rectPositions pixels pixels
    colR (x,y) = bilinearInterpolatedRect (x,y+1) (x+1,y)
      (plasmaCol (x) (y+1) t)
      (plasmaCol (x) (y) t)
      (plasmaCol (x+1) (y) t)
      (plasmaCol (x+1) (y+1) t)
    rectForms = map colR poss |> map (move (-pixels/2,-pixels/2))
  in
    [rect pixels pixels |> filled black] ++ rectForms
      |> group |> scale (200/pixels)

{-| Returns all integral positions inside a rect.
rectPositions 2 3 =
  [(0,0),(1,0),(0,1),(1,1),(0,2),(1,2)]
-}

rectPositions w h =
  let
    xs = [0..(w-1)]
    ys = [0..(h-1)]
    row y = map (flip (,) <| y) xs
  in
    map row ys |> concat

type PlasmaColConf1D = { weight:Float, scale:Float, timeScale:Float }
type PlasmaColConf = { x:PlasmaColConf1D, y:PlasmaColConf1D }

plasmaColConf1D : Float -> Float -> Float -> PlasmaColConf1D
plasmaColConf1D w s t = { weight=w, scale=s, timeScale=t }

plasmaColConf : PlasmaColConf1D -> PlasmaColConf1D -> PlasmaColConf
plasmaColConf xConf yConf = { x=xConf, y=yConf }

rConf : [ PlasmaColConf ]
rConf = [ plasmaColConf (plasmaColConf1D 2 1 4) (plasmaColConf1D 5 3 2)
        , plasmaColConf (plasmaColConf1D 7 4 1) (plasmaColConf1D 1 5 7) ]

gConf : [ PlasmaColConf ]
gConf = [ plasmaColConf (plasmaColConf1D 6 2 2) (plasmaColConf1D 4 6 8)
        , plasmaColConf (plasmaColConf1D 4 8 8) (plasmaColConf1D 8 2 5) ]

bConf : [ PlasmaColConf ]
bConf = [ plasmaColConf (plasmaColConf1D 1 5 7) (plasmaColConf1D 8 5 3)
        , plasmaColConf (plasmaColConf1D 2 4 2) (plasmaColConf1D 5 2 1) ]

colValFromConf1D : Float -> Float -> PlasmaColConf1D -> Float
colValFromConf1D p t conf =
  conf.weight * sin(conf.scale * p + conf.timeScale * t)

colValFromConf : Float -> Float -> Float -> PlasmaColConf -> Float
colValFromConf x y t conf =
  let
    xCol = colValFromConf1D x t conf.x
    yCol = colValFromConf1D y t conf.y
  in
    xCol + yCol

divisorForColConfs : [PlasmaColConf] -> Float
divisorForColConfs confs =
  sum <| map (\conf -> conf.x.weight + conf.x.weight) confs

colValFromConfs : Float -> Float -> Float -> [PlasmaColConf] -> Float
colValFromConfs x y t confs =
  (sum <| map (colValFromConf x y t) confs) / divisorForColConfs confs

plasmaCol : Float -> Float -> Float -> Color
plasmaCol xIn yIn tIn =
  let
    x = xIn / 5
    y = yIn / 5
    t = 0.004 * tIn
    rDiv = divisorForColConfs rConf
    gDiv = divisorForColConfs gConf
    bDiv = divisorForColConfs bConf
    r = colValFromConfs x y t rConf
    g = colValFromConfs x y t gConf
    b = colValFromConfs x y t bConf
  in
    rgb (127 + round (127 * r))
        (127 + round (127 * g))
        (127 + round (127 * b))

bilinearInterpolatedRect :
  (Float,Float) ->
  (Float,Float) ->
  Color ->
  Color ->
  Color ->
  Color ->
  Form
bilinearInterpolatedRect tl br
    (Color rtl gtl btl atl)
    (Color rbl gbl bbl abl)
    (Color rbr gbr bbr abr)
    (Color rtr gtr btr atr) =
  let
    ctl = rgba rtl gtl btl 1
    cbl = rgba rbl gbl bbl 1
    cbr = rgba rbr gbr bbr 1
    ctr = rgba rtr gtr btr 1
    tr = (fst br,snd tl)
    bl = (fst tl,snd br)
    g1cm = rgba ((rtl+rbr) `div` 2) ((gtl+gbr) `div` 2) ((btl+bbr) `div` 2) 0.7
    g2cm = rgba ((rtr+rbl) `div` 2) ((gtr+gbl) `div` 2) ((btr+bbl) `div` 2) 0.03

    w = fst br - fst tl
    h = snd tr - snd bl
    c = (fst bl + w/2, snd tr - h/2)
    (movex, movey) = c

    tln = (fst tl - fst c, snd tl - snd c)
    bln = (fst bl - fst c, snd bl - snd c)
    brn = (fst br - fst c, snd br - snd c)
    trn = (fst tr - fst c, snd tr - snd c)
    g1 = linear tln brn [(0,ctl), (0.5,g1cm), (1,cbr)]
    g2 = linear trn bln [(0,ctr), (0.5,g2cm), (1,cbl)]
  in
    group [
      gradient g1 (rect w h)
    , gradient g2 (rect w h)
    ] |> move c