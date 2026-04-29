{-# LANGUAGE PatternSynonyms #-}
-- | PatternSynonyms fixture.
--
-- 'patSynResult' destructures a 'Tree' through the 'Bin' pattern
-- synonym defined in 'PatSynData'.
module PatSynFix (patSynResult) where

import           PatSynData (Tree (..), pattern Bin)

patSynResult :: Int
patSynResult = case Bin TLeaf 7 TLeaf of
  Bin _ v _ -> v + 5000
  _         -> 0
