module Lib (add, factorial) where

add :: Int -> Int -> Int
add x y = x + y

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
