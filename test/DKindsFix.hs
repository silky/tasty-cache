{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
-- | DataKinds fixture.
--
-- 'Tag' is promoted to the type level; 'Tagged' is indexed by a tag.
-- 'describeTag' is restricted to @'Foo@-tagged values; the test asserts
-- whether mutations to 'Tag' propagate to a test of 'describeTag'.
module DKindsFix (Tag (..), Tagged (..), describeTag, dKindsResult) where

data Tag = Foo | Bar

newtype Tagged (t :: Tag) = Tagged Int

describeTag :: Tagged 'Foo -> String
describeTag (Tagged n) = "dkinds-foo-prefix " ++ show n

dKindsResult :: String
dKindsResult = describeTag (Tagged 5 :: Tagged 'Foo)
