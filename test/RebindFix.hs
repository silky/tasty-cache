{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax  #-}
-- | RebindableSyntax fixture.
--
-- The @if-then-else@ in 'rebindIf' desugars to a call of the local
-- 'ifThenElse', not 'Prelude.ifThenElse'.  No textual mention of
-- @ifThenElse@ in 'rebindIf'.
module RebindFix (rebindIf) where

import           Prelude (Bool (..), Int, fromInteger)
import qualified Prelude as P

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True  t _ = t P.+ 1000
ifThenElse False _ f = f P.+ 1000

rebindIf :: Int
rebindIf = if True then 1 else 2
