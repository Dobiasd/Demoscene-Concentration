module Common.Random where

{-| -}

import Common.Algorithms(splitAt,init)

-- [0, 999]
randomInt : Int -> (Int,Int)
randomInt i =
  let
    j   = 701 * i  `mod` 10001
    ans = (j - 1) `mod` 1000
  in
    (j, ans)

randomInts : Int -> Int -> [Int]
randomInts seed amount =
  let
    go _ (seed, l) =
      let (nextSeed, val) = randomInt seed
      in (nextSeed, val::l)
  in
    foldr go (seed+123, []) [1..amount] |> snd

-- [0.0, 1.0)
randomFloat : Float -> (Float,Float)
randomFloat seed =
  let
    intSeed = round (10000 * seed)
    (newIntSeed, intVal) = randomInt intSeed
    floatVal = toFloat (intVal-1) / 999
    newFloatSeed = toFloat newIntSeed / 10000
  in
    (newFloatSeed, floatVal)


randomFloats : Float -> Int -> [Float]
randomFloats seed amount =
  let
    go _ (seed, l) =
      let (nextSeed, val) = randomFloat seed
      in (nextSeed, val::l)
  in
    foldr go (seed+1.23, []) [1..amount] |> snd

shuffle : [Int] -> [a] -> [a]
shuffle (i::is) l =
  case l of
    (x::xs) ->
      let (firsts, rest) = splitAt (i `mod` length l + 1) l
      in (last firsts) :: shuffle is (init firsts ++ rest)
    x -> x