module Common where

type Named      a = { a | name:String }
type Positioned a = { a | x:Float, y:Float }
type Sized      a = { a | w:Float, h:Float }
type Boxed      a = Sized (Positioned a)

type Point = Positioned {}
type Box = Boxed {}

point : Float -> Float -> Point
point x y = {x=x, y=y}

box : Float -> Float -> Float -> Float -> Box
box x y w h = {x=x, y=y, w=w, h=h }