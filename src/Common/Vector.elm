module Common.Vector where

{-| 3D vector math.
-}

import Transform2D(Transform2D,matrix)
import Common.Types(Positioned,Point,point)

-- The more elegant definition:
-- type Vector = Positioned {}
-- leads to: Could not find 'Vector' when solving type constraints,
-- so Vector is defined on its own.
type Vector = {x:Float, y:Float, z:Float}

vector : Float -> Float -> Float -> Vector
vector x y z = { x=x, y=y, z=z }

{-| Euclidian distance to the origin. -}
dist : Positioned a -> Float
dist {x,y,z} = sqrt (x^2 + y^2 + z^2)

{-| Distance between two positioned things. -}
distTo : Positioned a -> Positioned b -> Float
distTo a b = dist <| a `subVec` b

{-| Angle in the x-y-plane. -}
angle2D : Positioned a -> Float
angle2D {x,y} = atan2 x y

{-| Return normalized vector with given angle in the x-y-plane. -}
vector2DFromAngle : Float -> Vector
vector2DFromAngle angle = vector (sin angle) (cos angle) 0

{-| A transformation matrix for 3 dimensions vectors. -}
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

{-| Transform a positioned thing by a matrix. -}
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


-- source: http://stackoverflow.com/questions/1114257/transform-a-triangle-to-another-triangle
{-| Calculate the transformation
bringing the first three points exactly onto the last three points . -}
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

{-| Project a 3D position onto the x y plane
with the virtual camera at the origin. -}
project2d : Positioned a -> Point
project2d {x,y,z} = point (100*x / (-z)) (100*y / (-z)) z

normalize : Positioned a -> Positioned a
normalize v = v `multVec` (1 / dist v)

scalarProd : Positioned a -> Positioned b -> Float
scalarProd v1 v2 = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z