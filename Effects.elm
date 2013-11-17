module Effects where

{-|
-}


import Plasma
import Starfield
import Particles
import Tunnel
import Lissajous
import Sinescroller

data Effect = APlasma       Plasma.State
            | AStarfield    Starfield.State
            | AParticles    Particles.State
            | ATunnel       Tunnel.State
            | ALissajous    Lissajous.State
            | ASinescroller Sinescroller.State

equalType : Effect -> Effect -> Bool

-- todo: use pairs for pattern matching
equalType e1 e2 =
  case (e1, e2) of
    (APlasma _      , APlasma _      ) -> True
    (AStarfield _   , AStarfield _   ) -> True
    (AParticles _   , AParticles _   ) -> True
    (ATunnel _      , ATunnel _      ) -> True
    (ALissajous _   , ALissajous _   ) -> True
    (ASinescroller _, ASinescroller _) -> True

-- todo: factory with string parameter?
makePlasma       = APlasma       <| Plasma.make
makeStarfield    = AStarfield    <| Starfield.make
makeParticles    = AParticles    <| Particles.make
makeTunnel       = ATunnel       <| Tunnel.make
makeLissajous    = ALissajous    <| Lissajous.make
makeSinescroller = ASinescroller <| Sinescroller.make

step : Float -> Effect -> Effect
step delta effect =
  case effect of
    APlasma       state -> APlasma       <| Plasma.step       delta state
    AStarfield    state -> AStarfield    <| Starfield.step    delta state
    AParticles    state -> AParticles    <| Particles.step    delta state
    ATunnel       state -> ATunnel       <| Tunnel.step       delta state
    ALissajous    state -> ALissajous    <| Lissajous.step    delta state
    ASinescroller state -> ASinescroller <| Sinescroller.step delta state

display : Effect -> Form
display effect =
  case effect of
    APlasma       state -> Plasma.display       state
    AStarfield    state -> Starfield.display    state
    AParticles    state -> Particles.display    state
    ATunnel       state -> Tunnel.display       state
    ALissajous    state -> Lissajous.display    state
    ASinescroller state -> Sinescroller.display state