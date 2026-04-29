{-# LANGUAGE ExplicitForAll #-}
-- | Polymorphic helpers used at @Foo@ in the test suite.  The bodies
-- never mention @Foo@ or @Instances@; the instance-method dependency
-- is purely through type-class resolution at the call site.
module Polymorphic
  ( poly
  , Greet (..)
  , Insult (..)
  , roast
  , pair
  ) where

-- The integer literal @1@ is desugared to @fromInteger 1 :: a@ at every
-- specialisation, so this body invokes three different methods of @Num a@:
-- @*@ and @+@ (textually present) plus @fromInteger@ (purely implicit).
-- The string @"fromInteger"@ never appears on the cache's BFS path.
poly :: Num a => a -> a
poly x = x * x + 1

-- A pair of single-method classes used together by 'roast'.  The
-- multi-constraint signature exercises the class-edge BFS for /two/
-- different classes from one test body.
class Greet a where
  greet :: a -> String

class Insult a where
  insult :: a -> String

roast :: (Greet a, Insult a) => a -> String
roast x = greet x ++ " and " ++ insult x

-- An explicit forall.  Used to verify that edits to the type signature
-- (e.g. swapping @forall a b@ for @forall b a@) invalidate the cache —
-- the order matters for @TypeApplications@ even when the body and the
-- value-level signature are otherwise identical.
pair :: forall a b. a -> b -> (a, b)
pair x y = (x, y)
