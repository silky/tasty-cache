-- | Diamond dependency scenario.
--
-- Dependency graph:
--
--   combined ─── partA ─┐
--            └── partB ─┴── base
--
-- Editing 'base' invalidates tests for 'base', 'partA', 'partB', and
-- 'combined' — the transitive BFS reaches all four from the test bodies.
module Diamond (base, partA, partB, combined) where

-- | Shared primitive used by both branches.
base :: Int -> Int
base n = n + 1

-- | Applies base twice.
partA :: Int -> Int
partA n = base (base n)

-- | Applies base once then doubles.
partB :: Int -> Int
partB n = base n * 2

-- | Combines both branches.
combined :: Int -> Int
combined n = partA n + partB n
