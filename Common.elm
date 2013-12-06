module Common where

import Transform2D(Transform2D,matrix)

type Named       a = { a | name:String }
type Positioned  a = { a | x:Float, y:Float, z:Float }
type Moving      a = { a | vx:Float, vy:Float, vz:Float }
type Sized       a = { a | w:Float, h:Float, d:Float }
type WithRadius  a = { a | r:Float }
type WithNormal  a = { a | nx:Float, ny:Float, nz:Float }
type Colored     a = { a | col:Color }
type Boxed       a = Sized (Positioned a)

type Point = Positioned {}
type Box = Boxed {}

point2D : Float -> Float -> Point
point2D x y = point x y 0

point2DtoPair : Positioned a -> (Float,Float)
point2DtoPair pt = (pt.x, pt.y)

point : Float -> Float -> Float -> Point
point x y z = {x=x, y=y, z=z}

box2D : Float -> Float -> Float -> Float -> Box
box2D x y w h = box x y 0 w h 0

box : Float -> Float -> Float -> Float -> Float -> Float -> Box
box x y z w h d = {x=x, y=y, z=z, w=w, h=h, d=d }









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














type Vector = {x:Float, y:Float, z:Float}

vector : Float -> Float -> Float -> Vector
vector x y z = { x=x, y=y, z=z }

dist : Positioned a -> Float
dist {x,y,z} = sqrt (x^2 + y^2 + z^2)

distTo : Positioned a -> Positioned b -> Float
distTo a b = dist <| a `subVec` b

angle2D : Positioned a -> Float
angle2D {x,y} = atan2 x y

vector2DFromAngle : Float -> Vector
vector2DFromAngle angle = vector (sin angle) (cos angle) 0

type Transform3D = { m11:Float, m12:Float, m13:Float, m14:Float
                   , m21:Float, m22:Float, m23:Float, m24:Float
                   , m31:Float, m32:Float, m33:Float, m34:Float
                   , m41:Float, m42:Float, m43:Float, m44:Float }

transform3D : Float -> Float -> Float -> Float
           -> Float -> Float -> Float -> Float
           -> Float -> Float -> Float -> Float
           -> Float -> Float -> Float -> Float
           -> Transform3D
transform3D m11 m12 m13 m14
            m21 m22 m23 m24
            m31 m32 m33 m34
            m41 m42 m43 m44 =
  { m11=m11, m12=m12, m13=m13, m14=m14
  , m21=m21, m22=m22, m23=m23, m24=m24
  , m31=m31, m32=m32, m33=m33, m34=m34
  , m41=m41, m42=m42, m43=m43, m44=m44 }

rotateX : Float -> Transform3D
rotateX a = transform3D 1    0         0     0
                        0 (cos a) (-(sin a)) 0
                        0 (sin a)   (cos a)  0
                        0    0         0     1

rotateY : Float -> Transform3D
rotateY a = transform3D   (cos a)  0 (sin a) 0
                            0      1   0     0
                        (-(sin a)) 0 (cos a) 0
                            0      0   0     1

rotateZ : Float -> Transform3D
rotateZ a = transform3D (cos a) (-(sin a)) 0 0
                        (sin a)   (cos a)  0 0
                           0         0     1 0
                           0         0     0 1

move3 : Vector -> Transform3D
move3 {x,y,z} = transform3D 1 0 0 x
                            0 1 0 y
                            0 0 1 z
                            0 0 0 1

applyTransform3D : Transform3D -> Positioned a -> Positioned a
applyTransform3D
    { m11, m12, m13, m14
    , m21, m22, m23, m24
    , m31, m32, m33, m34
    , m41, m42, m43, m44 }
    ({x,y,z} as thing) =
  { thing | x <- (m11*x + m12*y + m13*z) + m14
          , y <- (m21*x + m22*y + m23*z) + m24
          , z <- (m31*x + m32*y + m33*z) + m34 }

getAffineTransformation :
  (Float,Float) -> (Float,Float) -> (Float,Float) ->
  (Float,Float) -> (Float,Float) -> (Float,Float) ->
  (Transform2D, Transform3D)
getAffineTransformation
  (x11,x12) (x21,x22) (x31,x32)
  (y11,y12) (y21,y22) (y31,y32) =
  let
    a1 = ((y11-y21)*(x12-x32)-(y11-y31)*(x12-x22))/
         ((x11-x21)*(x12-x32)-(x11-x31)*(x12-x22))
    a2 = ((y11-y21)*(x11-x31)-(y11-y31)*(x11-x21))/
         ((x12-x22)*(x11-x31)-(x12-x32)*(x11-x21))
    a3 = y11-a1*x11-a2*x12
    a4 = ((y12-y22)*(x12-x32)-(y12-y32)*(x12-x22))/
         ((x11-x21)*(x12-x32)-(x11-x31)*(x12-x22))
    a5 = ((y12-y22)*(x11-x31)-(y12-y32)*(x11-x21))/
         ((x12-x22)*(x11-x31)-(x12-x32)*(x11-x21))
    a6 = y12-a4*x11-a5*x12
  in
    (matrix a1 a2 a4 a5 a3 a6,
      transform3D a1 a2  0 a3
                  a4 a5  0 a6
                   0  0  1  0
                   0  0  0  1)

crossProduct : Vector -> Vector -> Vector
crossProduct a b =
  Vector (a.y * b.z - b.y * a.z)
         (a.z * b.x - b.z * a.x)
         (a.x * b.y - b.x * a.y)

addVec : Vector -> Positioned a -> Positioned a
addVec b ({x,y,z} as p) =
  {p | x <- (x + b.x)
     , y <- (y + b.y)
     , z <- (z + b.z)}

subVec : Positioned a -> Positioned b -> Vector
subVec a b = Vector (a.x - b.x) (a.y - b.y) (a.z - b.z)

multVec : Positioned a -> Float -> Positioned a
multVec ({x,y,z} as a) f = { a | x <- x * f
                               , y <- y * f
                               , z <- z * f }



project2d : Positioned a -> Point
project2d {x,y,z} = point (100*x / (-z)) (100*y / (-z)) z

normalize : Positioned a -> Positioned a
normalize v = v `multVec` (1 / dist v)

scalarProd : Positioned a -> Positioned b -> Float
scalarProd v1 v2 = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z







type PositionedForm = Positioned {f:Form}

dummyForm = rect 0 0 |> filled (rgba 0 0 0 0)

decomposeColor : Color -> (Int,Int,Int,Float)
decomposeColor (Color r g b a) = (r,g,b,a)

positionedForm : Form -> Positioned a -> PositionedForm
positionedForm f {x,y,z} = { f=f, x=x, y=y, z=z }

displayPositionedForm : PositionedForm -> Form
displayPositionedForm {f,x,y} = f |> move (x, y)

isPosOK : Positioned a -> Bool
isPosOK {x,y,z} = z < -1 && x >= -100 && x <= 100 && y >= -100 && y <= 100

displayPositionedForms : [PositionedForm] -> Form
displayPositionedForms fs =
    fs
    |> sortBy (\a b -> a.z > b.z)
    |> filter isPosOK
    |> map displayPositionedForm |> group







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
