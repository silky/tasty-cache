{-# LANGUAGE PatternSynonyms #-}
-- | Companion module for 'PatSynFix'.  Declares a pattern synonym that
-- the user module destructures with.
module PatSynData (Tree (..), pattern Bin) where

data Tree = TLeaf | TNode Tree Int Tree

pattern Bin :: Tree -> Int -> Tree -> Tree
pattern Bin l v r = TNode l v r
