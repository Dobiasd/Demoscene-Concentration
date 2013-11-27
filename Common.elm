module Common where

type Named       a = { a | name:String }
type Positioned  a = { a | x:Float, y:Float }
type Sized       a = { a | w:Float, h:Float }
type Colored     a = { a | col:Color }
type Boxed       a = Sized (Positioned a)
type Positioned3 a = Positioned { a | z:Float }

type Point = Positioned {}
type Point3 = Positioned3 {}
type Box = Boxed {}

point : Float -> Float -> Point
point x y = {x=x, y=y}

point3 : Float -> Float -> Float -> Point3
point3 x y z = {x=x, y=y, z=z}

box : Float -> Float -> Float -> Float -> Box
box x y w h = {x=x, y=y, w=w, h=h }

{-| [a,b] [1,2] [x,y] -> [(a,1,x),(b,2,y)] -}
zip3 : [a] -> [b] -> [c] -> [(a,b,c)]
zip3 xs ys zs = case (xs, ys, zs) of
                  (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
                  otherwise                -> []

curry3 : ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

uncurry3 : (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c


{- [1,2,3,4,5,6,7,8] -> [(1,2,3),(4,5,6)] -}
nonOverlappingTriples : [a] -> [(a,a,a)]
nonOverlappingTriples l =
  case l of
    (x1::x2::x3::xs) -> (x1,x2,x3) :: nonOverlappingTriples xs
    _             -> []


-- todo: for all comparables
-- todo free predicate
quicksort : [Float] -> [Float]
quicksort l =
  case l of
    [] -> []
    (p::xs) ->
      let
        lesser  = filter ((>=) p) xs
        greater = filter ((<) p) xs
      in
        (quicksort lesser) ++ [p] ++ (quicksort greater)

type Vector = { x:Float, y:Float, z:Float }

vector : Float -> Float -> Float -> Vector
vector x y z = { x=x, y=y, z=z }

dist : Vector -> Float
dist {x,y,z} = sqrt (x^2 + y^2 + z^2)

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

applyTransform3D : Transform3D -> Vector -> Vector
applyTransform3D
    { m11, m12, m13, m14
    , m21, m22, m23, m24
    , m31, m32, m33, m34
    , m41, m42, m43, m44 }
    {x,y,z} =
  vector
    (m11*x + m12*y + m13*z)
    (m21*x + m22*y + m23*z)
    (m31*x + m32*y + m33*z)


type Face = { tl:Vector, tr:Vector, bl:Vector }

face : Vector -> Vector -> Vector -> Face
face tl tr bl = { tl=tl, tr=tr, bl=bl }

addVec : Vector -> Vector -> Vector
addVec a b = Vector (a.x + b.x) (a.y + b.y) (a.y + b.y)

subVec : Vector -> Vector -> Vector
subVec a b = Vector (a.x - b.x) (a.y - b.y) (a.y - b.y)

faceBr : Face -> Vector
faceBr {tl,tr,bl} = (tr `subVec` tl) `addVec` bl

cubeFaces = [ Face (Vector (-100) ( 100) ( 100))
                   (Vector ( 100) ( 100) ( 100))
                   (Vector (-100) (-100) ( 100))
            , Face (Vector ( 100) ( 100) (-100))
                   (Vector (-100) ( 100) (-100))
                   (Vector ( 100) (-100) (-100))
            , Face (Vector ( 100) ( 100) ( 100))
                   (Vector ( 100) ( 100) (-100))
                   (Vector ( 100) (-100) ( 100))
            , Face (Vector (-100) ( 100) (-100))
                   (Vector (-100) ( 100) ( 100))
                   (Vector (-100) (-100) (-100))
            , Face (Vector (-100) ( 100) (-100))
                   (Vector ( 100) ( 100) (-100))
                   (Vector (-100) ( 100) ( 100))
            , Face (Vector (-100) (-100) ( 100))
                   (Vector ( 100) (-100) ( 100))
                   (Vector (-100) (-100) (-100))
            ]


transformFace : Transform3D -> Face -> Face
transformFace matrix {tl,tr,bl} =
  let f = applyTransform3D matrix
  in face (f tl) (f tr) (f bl)

transformFaces : Transform3D -> [Face] -> [Face]
transformFaces matrix = map (transformFace matrix)




random : Float -> (Float,Float)
random seed =
  let
    f seed = 0.5 + 0.5 * cos (seed + 7.312)
    val = f seed
    nextSeed = seed * seed + 1.23456 + cos seed
  in
    (nextSeed, val)

randoms : Float -> Int -> [Float]
randoms seed amount =
  let
    go _ (seed, l) =
      let (nextSeed, val) = random seed
      in (nextSeed, val::l)
  in
    foldr go (seed, []) [0..amount] |> snd