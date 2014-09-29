module Effects.Sinescroller where

{-| Generates a sine scroller effect.
-}

import Effects.Cube as Cube
import Effects.Effect as Eff
import Common.Types(Point,Colored,Positioned,point2D)
import Common.Algorithms(uncurry3,floatMod)

import List
import String
import Text

charDist = 14
timeFactX = 0.1
timeFactY = 0.002
textPosFact = -0.2

type State = {message:String, time:Float, cube:Eff.Effect}

sinescroller : State -> Eff.Effect
sinescroller s = Eff.Effect
  {step=step s, display=display s, name="Sinescroller"}

make : String -> Eff.Effect
make message = sinescroller {message=message
                           , time=0
                           , cube=Cube.make [] (rgba 255 255 255 0.07)}

step : State -> Float -> Eff.Effect
step ({time, cube} as state) delta =
    sinescroller { state | time <- time + delta
                         , cube <- Eff.step cube delta }

deconcat : String -> [String]
deconcat = String.foldr (\c acc -> String.fromList [c] :: acc) []

charPos : Float -> Int -> Point
charPos time textPos =
  point2D
    (toFloat textPos * charDist - time * timeFactX)
    (70 * sin (toFloat textPos * textPosFact + timeFactY*time))

charCol : Float -> Positioned a -> Color
charCol time {x,y} = hsl (0.001 * time + 0.03 * x) 1 0.5

type ScrollerChar = Colored ( Positioned {s:String} )

scrollerChar : String -> Point -> Color -> ScrollerChar
scrollerChar s pos col = {s=s, x=pos.x, y=pos.y, z=0, col=col}

wrapCharPos : Float -> ScrollerChar -> ScrollerChar
wrapCharPos minX ({x} as sc) =
    { sc | x <- 100 + floatMod (x) (minX) }

scrollerChars a b c = (map (uncurry3 scrollerChar)) (zip3 a b c)

txt : Color -> String -> Element
--txt c = text . (Text.height 28) . monospace . Text.color c . toText
txt c = toText >> Text.color c >> monospace >> Text.height 28 >> leftAligned

displayScrollerChar : ScrollerChar -> Form
displayScrollerChar {s,x,y,col} = txt col s |> toForm |> move (x,y)

display : State -> Form
display ({time,cube,message} as state) =
  let
    len = String.length message
    positions = map (charPos time) [0..len]
    colors = map (charCol time) positions
    chars = scrollerChars (deconcat message) positions colors
    wrappedChars = map (wrapCharPos <| -charDist * (14 + toFloat len)) chars
    goodChars = filter (\{x} -> x > -93 && x < 93) wrappedChars
    charsForm = map displayScrollerChar goodChars |> group
  in
    group [ rect 200 200 |> filled (rgb 0 0 0)
          , Eff.display cube
          , charsForm ]