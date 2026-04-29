-- | Companion module for 'StdDerivFix'.  Defines the data type whose
-- 'Show' instance is derived in a different module via
-- @StandaloneDeriving@.
module StdDerivData (StdData (..)) where

data StdData = StdLeaf | StdBranch StdData StdData
