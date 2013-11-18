module Effect where

{-|
-}

data Effect = Effect {step:(Float -> Effect), display:Form, name:String}

equalType : Effect -> Effect -> Bool
equalType (Effect e1) (Effect e2) = e1.name == e2.name