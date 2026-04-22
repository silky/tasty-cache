-- | Mutual recursion scenario.
--
-- isEven and isOdd call each other.  The transitive BFS means that editing
-- either function invalidates tests for both, since each is reachable from
-- the other via the HIE identifier graph.
module Parity (isEven, isOdd, collatz) where

isEven :: Int -> Bool
isEven 0 = True
isEven n = isOdd (n - 1)

isOdd :: Int -> Bool
isOdd 0 = False
isOdd n = isEven (n - 1)

-- | Steps to reach 1 via the Collatz process.  Independent of isEven/isOdd.
collatz :: Int -> Int
collatz 1 = 0
collatz n
  | even n    = 1 + collatz (n `div` 2)
  | otherwise = 1 + collatz (3 * n + 1)
