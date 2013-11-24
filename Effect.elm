module Effect where

{-|
-}

import Common(Named)

data Effect = Effect (Named {step:(Float -> Effect), display:Form})

equalType : Effect -> Effect -> Bool
equalType (Effect e1) (Effect e2) = e1.name == e2.name

step : Effect -> Float -> Effect
step (Effect ef) = ef.step

display : Effect -> Form
display (Effect ef) = ef.display