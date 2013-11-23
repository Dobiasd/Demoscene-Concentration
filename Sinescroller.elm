module Sinescroller where

{-| Generates a sine scroller effect.

@docs sinescroller
-}

import Effect(Effect, effect)
import Common(Colored,Positioned,Point,point,zip3,uncurry3)

import String

charDist = 14

type State = {time:Float}

message = "Greetings go out to everybody loving the demoscene and functional programming. ;-)   -   daiw.de"

sinescroller : State -> Effect
sinescroller s = Effect {step=step s, display=display s, name="Sinescroller"}

make : Effect
make = sinescroller {time=0}

step : State -> Float -> Effect
step ({time} as state) delta = sinescroller { state | time <- time + delta }

deconcat : String -> [String]
deconcat = String.foldr (\c acc -> String.fromList [c] :: acc) []

charPos : Float -> Int -> Point
charPos time textPos =
  let
    timeFactX = 0.1
    timeFactY = 0.002
    textPosFact = -0.2
  in
    point
      (toFloat textPos * charDist - time * timeFactX)
      (70 * sin (toFloat textPos * textPosFact + timeFactY*time))

txt : Color -> String -> Form
txt col =
  toForm . text . (Text.height 28) . monospace . Text.color col . toText

charCol : Float -> Positioned a -> Color
charCol time {x,y} = hsv (0.001 * time + 0.03 * x) 1 1

type ScrollerChar = Colored ( Positioned {s:String} )

scrollerChar : String -> Point -> Color -> ScrollerChar
scrollerChar s pos col = {s=s, x=pos.x, y=pos.y, col=col}

floatMod : Float -> Float -> Float
floatMod numerator divisor =
  let
    q = numerator / divisor |> floor |> toFloat
  in
    numerator - q * divisor

wrapCharPos : Float -> ScrollerChar -> ScrollerChar
wrapCharPos minX ({x} as sc) =
    { sc | x <- 100 + floatMod (x) (minX) }

scrollerChars a b c = (map (uncurry3 scrollerChar)) (zip3 a b c)

displayScrollerChar : ScrollerChar -> Form
displayScrollerChar {s,x,y,col} = txt col s |> move (x,y)

{-| Returns a sine scroller effect filled form
depending on the current state. -}
display : State -> Form
display ({time} as state) =
  let
    len = String.length message
    poss : [Point]
    poss = map (charPos time) [0..len]
    cols = map (charCol time) poss
    charStrings = deconcat message
    chars = scrollerChars charStrings poss cols
    wrappedChars = map (wrapCharPos <| -charDist * (14 + toFloat len)) chars
    goodChars = filter (\{x} -> x > -80 && x < 80) wrappedChars
    forms = map displayScrollerChar goodChars
  in
    [rect 200 200 |> filled (rgb 0 0 0)] ++ forms |> group