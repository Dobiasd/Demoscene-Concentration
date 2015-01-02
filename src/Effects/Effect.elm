module Effects.Effect where

{-| Implements base class style function dispatching according to this
[article](https://github.com/Dobiasd/articles/blob/master/from_oop_to_fp_-_inheritance_and_the_expression_problem.md).

Every effect has to implement the convention to return its displayed form
in the range of [-100,+100] for both dimensions.
-}

import Color(rgb)
import Common.Types(Named)
import Graphics.Collage(Form, rect, filled)

type Effect = Effect (Named {step : (Float -> Effect), display : Form})

step : Effect -> Float -> Effect
step (Effect ef) = ef.step

display : Effect -> Form
display effect = case effect of
                   (Effect ef) -> ef.display
                   _           -> rect 0 0 |> filled (rgb 0 0 0)