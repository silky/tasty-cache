{-# LANGUAGE OverloadedStrings #-}
-- | OverloadedStrings fixture.
--
-- A string literal in 'greetOvStr' desugars to 'fromString \"world\"',
-- dispatched through the 'IsString S' dictionary.  No textual mention of
-- @fromString@ on the BFS path; reaching the instance method requires
-- the class-edge BFS.
module OvStrFix (S(..), greetOvStr) where

import Data.String (IsString(..))

newtype S = S String deriving (Eq, Show)

instance IsString S where
  fromString s = S ("ovstr:" ++ s)

greetOvStr :: S
greetOvStr = "world"
