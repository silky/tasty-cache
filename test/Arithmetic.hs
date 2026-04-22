{-# LANGUAGE TemplateHaskell #-}
-- | Template Haskell scenario.
--
-- Dependency structure:
--   add5\/add10 depend on 'adderExpr' in Meta.hs
--   timesBy3   depends on 'doublerExpr' in Meta.hs
--
-- Editing 'adderExpr' invalidates the add5\/add10 tests but not timesBy3,
-- because the HIE graph records which template each splice uses.
module Arithmetic (add5, add10, timesBy3) where

import           Meta (adderExpr, doublerExpr)

add5 :: Int -> Int
add5 = $(adderExpr 5)

add10 :: Int -> Int
add10 = $(adderExpr 10)

timesBy3 :: Int -> Int
timesBy3 = $(doublerExpr 3)
