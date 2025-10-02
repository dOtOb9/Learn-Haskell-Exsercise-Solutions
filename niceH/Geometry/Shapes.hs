module Shapes where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float deriving (Show)
