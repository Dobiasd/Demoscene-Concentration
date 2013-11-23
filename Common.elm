module Common where

type Named      a = { a | name:String }
type Positioned a = { a | x:Float, y:Float }
type Sized      a = { a | w:Float, h:Float }
type Colored    a = { a | col:Color }
type Boxed      a = Sized (Positioned a)

type Point = Positioned {}
type Box = Boxed {}

point : Float -> Float -> Point
point x y = {x=x, y=y}

box : Float -> Float -> Float -> Float -> Box
box x y w h = {x=x, y=y, w=w, h=h }

{-| [a,b] [1,2] [x,y] -> [(a,1,x),(b,2,y)] -}
zip3 : [a] -> [b] -> [c] -> [(a,b,c)]
zip3 xs ys zs = case (xs, ys, zs) of
                  (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
                  otherwise -> []

curry3 : ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

uncurry3 : (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c