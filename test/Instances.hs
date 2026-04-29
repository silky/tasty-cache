-- | Standalone instances, defined in a module that the @Polymorphic@
-- test body never names directly.  Used both to exercise the
-- class-edge BFS for the @Foo@ tests and to demonstrate (in
-- @test\/FalseNegatives.hs@) that the BFS is conservative across
-- instances of the same class and across multiple classes from a
-- single test body.
module Instances (Foo (..), Bar (..)) where

import           Polymorphic (Greet (..), Insult (..))

newtype Foo = Foo { unFoo :: Int }
  deriving stock (Eq, Show)

newtype Bar = Bar { unBar :: Int }
  deriving stock (Eq, Show)

instance Num Foo where
  Foo a + Foo b = Foo (a + b)
  Foo a * Foo b = Foo (a * b)
  abs    (Foo a) = Foo (abs a)
  signum (Foo a) = Foo (signum a)
  fromInteger n  = Foo (fromInteger n)
  negate (Foo a) = Foo (negate a)

instance Num Bar where
  Bar a + Bar b = Bar (a + b)
  Bar a * Bar b = Bar (a * b)
  abs    (Bar a) = Bar (abs a)
  signum (Bar a) = Bar (signum a)
  fromInteger n  = Bar (fromInteger n)
  negate (Bar a) = Bar (negate a)

instance Greet Foo where
  greet (Foo n) = "Hello Foo " ++ show n

instance Insult Foo where
  insult (Foo n) = "Foo " ++ show n ++ " is silly"
