module Plasma where

{-| Generates a plasma effect.

@docs plasma
-}




{-| Returns a plasma effect filled form depending on the current time. -}
plasma : Time -> Form
plasma t =
  let
    fac = 25
    xs = map ((*)fac) [-4..3]
    ys = map ((*)fac) [-4..3]
    row y = map (\x -> (x,y)) xs
    poss = map row ys |> concat
    r (x,y) ctl cbl cbr ctr =
      bilinearInterpolatedRect (x,y+fac) (x+fac,y) ctl cbl cbr ctr
    pc = plasmaCol
    colR (x,y) = r (x,y)
      (pc (x) (y+fac) t)
      (pc (x) (y) t)
      (pc (x+fac) (y) t)
      (pc (x+fac) (y+fac) t)
    rs = map colR poss
  in
    group <| [rect 200 200 |> filled black] ++ rs


plasmaCol : Float -> Float -> Float -> Color
plasmaCol x y t = hsv (sin (0.0102*(x+0.11*t)) + sin(0.0087*(y+0.13*t))
                     + sin (0.0021*(x+0.34*t)) + sin(0.0187*(y-0.23*t))
                     + sin (0.0162*(x-0.47*t)) + sin(0.0017*(y+0.33*t))
                     + sin (0.0061*(x-0.57*t)) + sin(0.0106*(y-0.43*t))
                    ) 1 1

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
