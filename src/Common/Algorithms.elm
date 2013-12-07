module Common.Algorithms where

{-| -}

{-| [a,b] [1,2] [x,y] -> [(a,1,x),(b,2,y)] -}
zip3 : [a] -> [b] -> [c] -> [(a,b,c)]
zip3 xs ys zs = case (xs, ys, zs) of
                  (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
                  otherwise                -> []

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

init : [a] -> [a]
init l = case l of
           [x] -> []
           (x::xs) -> x :: init xs

splitAt : Int -> [a] -> ([a], [a])
splitAt n l = case (n, l) of
                (0, xs)     -> ([], xs)
                (_, [])     -> ([], [])
                (n, (x::xs)) ->
                  let (xs', xs'') = splitAt (n - 1) xs
                  in (x::xs', xs'')

pairs : [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

numberedPairs : [a] -> [(Int,a,a)]
numberedPairs xs =
  let
    ps = pairs xs
    l = length ps - 1
    nums = [0..l]
  in
    zipWith (\i (a,b) -> (i,a,b)) nums ps


{- [1,2,3,4,5,6,7,8] -> [(1,2,3),(4,5,6)] -}
nonOverlappingTriples : [a] -> [(a,a,a)]
nonOverlappingTriples l =
  case l of
    (x1::x2::x3::xs) -> (x1,x2,x3) :: nonOverlappingTriples xs
    _             -> []

{- [1,2,3,4,5,6,7,8,9] -> [(1,2,3,4),(5,6,7,8)] -}
nonOverlappingQuadruples : [a] -> [(a,a,a,a)]
nonOverlappingQuadruples l =
  case l of
    (x1::x2::x3::x4::xs) -> (x1,x2,x3,x4) :: nonOverlappingQuadruples xs
    _                    -> []

{- [1,2,3,4,5,6,7,8,9,10,11,12,13,14] -> [(1,2,3,4,5,6),(7,8,9,10,11,12)] -}
nonOverlappingSextuples : [a] -> [(a,a,a,a,a,a)]
nonOverlappingSextuples l =
  case l of
    (x1::x2::x3::x4::x5::x6::xs) -> (x1,x2,x3,x4,x5,x6) :: nonOverlappingSextuples xs
    _                            -> []

quicksort : (a -> a -> Bool) -> [a] -> [a]
quicksort cmp l =
  case l of
    [] -> []
    (p::xs) ->
      let
        lesser  = filter (cmp p) xs
        greater = filter (not . (cmp p)) xs
      in
        (quicksort cmp lesser) ++ [p] ++ (quicksort cmp greater)

sortBy = quicksort