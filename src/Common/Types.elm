module Common.Types where

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

point : Float -> Float -> Float -> Point
point x y z = { x=x, y=y, z=z }

point2D : Float -> Float -> Point
point2D x y = point x y 0

point2DtoPair : Positioned a -> (Float,Float)
point2DtoPair pt = (pt.x, pt.y)

box2D : Float -> Float -> Float -> Float -> Box
box2D x y w h = box x y 0 w h 0

box : Float -> Float -> Float -> Float -> Float -> Float -> Box
box x y z w h d = {x=x, y=y, z=z, w=w, h=h, d=d }

{-| Check if a position in inside a box. -}
inBox : Positioned a -> Boxed b -> Bool
inBox pos box =
  let
    xDist = abs (pos.x - box.x)
    yDist = abs (pos.y - box.y)
  in
    xDist <= box.w/2 && yDist <= box.h/2