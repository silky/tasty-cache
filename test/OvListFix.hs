{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
-- | OverloadedLists fixture.
--
-- A list literal in 'sampleOvList' desugars to 'fromListN'/'fromList'
-- via the 'IsList L' dictionary.  Class-edge BFS test territory.
module OvListFix (L(..), sampleOvList) where

import GHC.Exts (IsList(..))

newtype L = L [Int] deriving (Eq, Show)

instance IsList L where
  type Item L = Int
  fromList xs = L (xs ++ [99999])
  toList (L xs) = xs

sampleOvList :: L
sampleOvList = [1,2,3]
