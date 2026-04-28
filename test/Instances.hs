-- | A standalone @Num Foo@ instance, defined in a module that the
-- @Polymorphic@ test body never names directly.  Used to demonstrate the
-- instance-resolution false negative in the cache.
module Instances (Foo (..)) where

newtype Foo = Foo { unFoo :: Int }
  deriving stock (Eq, Show)

instance Num Foo where
  Foo a + Foo b = Foo (a + b)
  Foo a * Foo b = Foo (a * b)
  abs    (Foo a) = Foo (abs a)
  signum (Foo a) = Foo (signum a)
  fromInteger n  = Foo (fromInteger n)
  negate (Foo a) = Foo (negate a)
