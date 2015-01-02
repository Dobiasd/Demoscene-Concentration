module Common.Algorithms where

{-| Commonly used Algorithms.
-}

import List((::), map2, tail, length, filter)

curry3 : ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

uncurry3 : (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

uncurry4 : (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d

floatMod : Float -> Float -> Float
floatMod numerator divisor =
  let
    q = numerator / divisor |> floor |> toFloat
  in
    numerator - q * divisor

init : List a -> List a
init l = case l of
           [x] -> []
           (x::xs) -> x :: init xs

{-| splitAt 2 [1,2,3,4,5,6] === ([1,2],[3,4,5,6]) -}
splitAt : Int -> List a -> (List a, List a)
splitAt n l = case (n, l) of
                (0, xs)     -> ([], xs)
                (_, [])     -> ([], [])
                (n, (x::xs)) ->
                  let (xs', xs'') = splitAt (n - 1) xs
                  in (x::xs', xs'')

{-| pairs [1,2,3,4,5] === [(1,2),(2,3),(3,4),(4,5)] -}
pairs : List a -> List (a,a)
pairs xs = map2 (,) xs (tail xs)

{-| numberedPairs [a,b,c,d,e] === [(0,a,b),(1,b,c),(2,c,d),(3,d,e)] -}
numberedPairs : List a -> List (Int,a,a)
numberedPairs xs =
  let
    ps = pairs xs
    l = length ps - 1
    nums = [0..l]
  in
    map2 (\i (a,b) -> (i,a,b)) nums ps

{-| nonOverlappingPairs [1,2,3,4,5] === [(1,2),(3,4)] -}
nonOverlappingPairs : List a -> List (a,a)
nonOverlappingPairs l =
  case l of
    (x1::x2::xs) -> (x1,x2) :: nonOverlappingPairs xs
    _            -> []

{-| nonOverlappingTriples [1,2,3,4,5,6,7,8] === [(1,2,3),(4,5,6)] -}
nonOverlappingTriples : List a -> List (a,a,a)
nonOverlappingTriples l =
  case l of
    (x1::x2::x3::xs) -> (x1,x2,x3) :: nonOverlappingTriples xs
    _             -> []

{-| nonOverlappingQuadruples [1,2,3,4,5,6,7,8,9] === [(1,2,3,4),(5,6,7,8)] -}
nonOverlappingQuadruples : List a -> List (a,a,a,a)
nonOverlappingQuadruples l =
  case l of
    (x1::x2::x3::x4::xs) -> (x1,x2,x3,x4) :: nonOverlappingQuadruples xs
    _                    -> []

{-| Sort list with given comparison function. -}
quicksort : (a -> a -> Bool) -> List a -> List a
quicksort cmp l =
  case l of
    [] -> []
    (p::xs) ->
      let
        lesser  = filter (cmp p) xs
        greater = filter (cmp p >> not) xs
      in
        (quicksort cmp lesser) ++ [p] ++ (quicksort cmp greater)

sortByLess = quicksort