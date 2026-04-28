-- | A polymorphic helper used at @Foo@ in the test suite.  The body never
-- mentions @Foo@ or @Instances@; the instance-method dependency is purely
-- through type-class resolution at the call site.
module Polymorphic (poly) where

-- The integer literal @1@ is desugared to @fromInteger 1 :: a@ at every
-- specialisation, so this body invokes three different methods of @Num a@:
-- @*@ and @+@ (textually present) plus @fromInteger@ (purely implicit).
-- The string @"fromInteger"@ never appears on the cache's BFS path.
poly :: Num a => a -> a
poly x = x * x + 1
