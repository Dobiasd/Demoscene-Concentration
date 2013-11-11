module Plasma where

{-| Generates a plasma effect.

@docs plasma
-}





{-| Returns a plasma effect filled form depending on the current time. -}

--plasma t = rect 200 200 |> filled (rgb 255 0 0)


plasma : Time -> Form
plasma t =
  let
    fac = 50
    xs = map ((*)fac) [-2..2]
    ys = map ((*)fac) [-2..2]
    row y = map (\x -> (x,y)) xs
    poss = map row ys |> concat
    -- todo adjust col order according to positive y axis
    r (x,y) ctl cbl cbr ctr =
      bilinearInterpolatedRect (x-fac,y+fac) (x+fac,y-fac) ctl cbl cbr ctr
    pc = plasmaCol
    colR (x,y) = r (x,y)
      (pc (x-fac) (y+fac) t)
      (pc (x-fac) (y-fac) t)
      (pc (x+fac) (y-fac) t)
      (pc (x+fac) (y+fac) t)
    rs = map colR poss
  in
    group <| [rect 200 200 |> filled black] ++ rs


old_plasma : Time -> Form
old_plasma t = bilinearInterpolatedRect (-100,-100) (100,100)
             (rgb 255   0   0)
             (rgb   0 255   0)
             (rgb   0   0 255)
             (rgb 255 255   0)
--             (rgb   0   0 255)
--             (rgb 255   0 255)
-- todo: why brackets and not <|?

plasmaCol : Float -> Float -> Float -> Color
plasmaCol x y t = hsv (sin (0.005*(x+0.006*t)) + sin(0.007*(y+0.008*t))) 1 1

-- todo
-- r _
-- _ b
-- a = (r+b)/2
-- [(0.0, r r r 1),(0.5, a a a 0),(1.0, b b b 1)]

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
    g1 = linear tl br [(0,ctl), (0.5,g1cm), (1,cbr)]
    g2 = linear tr bl [(0,ctr), (0.5,g2cm), (1,cbl)]
    w = fst br - fst tl
    h = snd tr - snd bl
    c = (fst bl + w/2, snd tr + h/2)
  in
    group [
      gradient g1 (rect w h)
    , gradient g2 (rect w h)
    --, gradient g1 (rect 200 200)
    ] |> move c

old_bilinearInterpolatedRect :
  (Float,Float) ->
  (Float,Float) ->
  Color ->
  Color ->
  Color ->
  Color ->
  Form
old_bilinearInterpolatedRect tl br -- ctl cbl cbr ctr =
    (Color rtl gtl btl atl)
    (Color rbl gbl bbl abl)
    (Color rbr gbr bbr abr)
    (Color rtr gtr btr atr) =
  let
    ctl = rgba rtl gtl btl 0
    cbl = rgba rbl gbl bbl 0
    cbr = rgba rbr gbr bbr 0
    ctr = rgba rtr gtr btr 0
    tr = (fst br,snd tl)
    bl = (fst tl,snd br)
    g1cm = rgb ((rtl+rbr) `div` 2) ((gtl+gbr) `div` 2) ((btl+bbr) `div` 2)
    g2cm = rgb ((rtr+rbl) `div` 2) ((gtr+gbl) `div` 2) ((btr+bbl) `div` 2)
    g1 = linear tl br [(0,ctl), (0.5,g1cm), (1,cbr)]
    g2 = linear tr bl [(0,ctr), (0.5,g2cm), (1,cbl)]
  in
    group [
      rect 200 200 |> filled black
    , gradient g1 (rect 200 200)
    --, gradient g2 (rect 200 200)
    ]

horizontallyInterpolatedRect :
  (Float,Float) ->
  (Float,Float) ->
  Color ->
  Color ->
  Form
horizontallyInterpolatedRect tl br (Color r g b a) cr =
  group [
  ]

verticallyInterpolatedRect :
  (Float,Float) ->
  (Float,Float) ->
  Color ->
  Color ->
  Form
verticallyInterpolatedRect tl br ct cb =
  group [
  ]