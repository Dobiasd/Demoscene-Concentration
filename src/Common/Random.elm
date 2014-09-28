module Common.Random where

{-| Some very primitive pseudo random generation.
  -}

import Common.Algorithms(splitAt,init)

{-| Generate new seed and random number in [0, 999] with given seed. -}
randomInt : Int -> (Int,Int)
randomInt i =
  let
    j   = 701 * i  % 10001
    ans = (j - 1) % 1000
  in
    (j, ans)

{-| Make amount numbers in [0, 999]. -}
randomInts : Int -> Int -> [Int]
randomInts seed amount =
  let
    go _ (seed, l) =
      let (nextSeed, val) = randomInt seed
      in (nextSeed, val::l)
  in
    foldr go (seed+123, []) [1..amount] |> snd

{-| Generate new seed and random number in [0.0, 1.0) with given seed. -}
randomFloat : Float -> (Float,Float)
randomFloat seed =
  let
    intSeed = round (10000 * seed)
    (newIntSeed, intVal) = randomInt intSeed
    floatVal = toFloat (intVal-1) / 999
    newFloatSeed = toFloat newIntSeed / 10000
  in
    (newFloatSeed, floatVal)

{-| Make amount numbers in [0.0, 1.0). -}
randomFloats : Float -> Int -> [Float]
randomFloats seed amount =
  let
    go _ (seed, l) =
      let (nextSeed, val) = randomFloat seed
      in (nextSeed, val::l)
  in
    foldr go (seed+1.23, []) [1..amount] |> snd

{-| Suffle a list using random numbers
The length of the list of random numbers has to be at least
as long as the list that shall be shuffled. -}
shuffle : [Int] -> [a] -> [a]
shuffle ((i::is) as randoms) l =
  case l of
    (x::xs) ->
      let (firsts, rest) = splitAt (i % length l + 1) l
      in (last firsts) :: shuffle is (init firsts ++ rest)
    x -> x