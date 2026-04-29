{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | GeneralizedNewtypeDeriving fixture.
--
-- 'GNDThing' gets 'Speaker2' coerced through the 'Underlying' instance.
-- The derived methods have no source span at the deriving site; their
-- bodies live in the underlying instance.
module GNDFix (GNDThing (..), Speaker2 (..), Underlying (..), gndResult) where

class Speaker2 a where
  bark :: a -> String

newtype Underlying = Underlying Int

instance Speaker2 Underlying where
  bark (Underlying n) = "gnd-bark " ++ show n

newtype GNDThing = GNDThing Underlying
  deriving newtype Speaker2

gndResult :: String
gndResult = bark (GNDThing (Underlying 5))
