module Shapes where

-- Define a new `Shape` data type

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

-- Pattern matching ~ Dynamic dispatch
-- in case of algebraic data types
-- http://en.wikipedia.org/wiki/Dynamic_dispatch

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

-- area $ Circle 10
-- area $ Rect 10 10
