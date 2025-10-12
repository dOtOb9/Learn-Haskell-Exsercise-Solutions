module Shapes ( Point, Shape, area, nudge, baseCircle, baseRectangle ) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float 
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2))
    = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Float -> Float -> Shape -> Shape
nudge dx dy (Circle (Point x y) r) = Circle (Point (x + dx) (y + dy)) r
nudge dx dy (Rectangle (Point x1 y1) (Point x2 y2)) = Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)
