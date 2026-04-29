{-# LANGUAGE TypeFamilies #-}
-- | TypeFamilies fixture.
--
-- @Element@ is an open type family; its instance for @[a]@ is what
-- makes @listElem@ type-check.  The interesting question is whether
-- editing the type-family instance RHS (which has no value-edge to any
-- binding) invalidates the test that uses @listElem@.
module TyFamFix (Element, listElem, tyFamResult) where

type family Element c
type instance Element [a] = a

listElem :: [Int] -> Element [Int]
listElem (x:_) = x + 7000
listElem []    = 0

tyFamResult :: Int
tyFamResult = listElem [1,2,3]
