{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | StandaloneDeriving fixture.
--
-- The 'Show' instance for 'StdData' is generated here, in a different
-- module from the data type's declaration.
module StdDerivFix (stdDerivResult) where

import           StdDerivData (StdData (..))

deriving instance Show StdData

stdDerivResult :: String
stdDerivResult = "stdderiv-prefix " ++ show (StdBranch StdLeaf StdLeaf)
