-- | Tests demonstrating false-negative scenarios in the caching logic.
--
-- A false negative means a test is served from cache (instant pass) when it
-- should have been re-run.  Each test group below isolates one mechanism by
-- which this can happen.
module FalseNegatives (falseNegativeTests) where

import qualified Data.ByteString.Char8        as BSC
import qualified Data.Map.Strict              as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Test.Tasty
import           Test.Tasty.HUnit

import           Test.Tasty.HieCache.Internal (countIndent, findExprEnd,
                                               pathKey)

falseNegativeTests :: TestTree
falseNegativeTests = testGroup "False-negative scenarios"
  [ stalenessTests
  , leafMapTests
  , findExprEndTests
  , instanceResolutionTests
  ]

-- ---------------------------------------------------------------------------
-- 1. The Nothing == Nothing staleness bug
--
-- The original staleness check was:
--
--   Map.lookup key fps /= Map.lookup key cache
--
-- When a test has no fingerprint (fps lookup = Nothing) and the cache is
-- empty (cache lookup = Nothing), this compares Nothing /= Nothing = False,
-- so the test is treated as fresh and replaced with an instant-pass
-- CachedTest — without ever running.

stalenessTests :: TestTree
stalenessTests = testGroup "Staleness logic"
  [ testCase "old logic: missing fingerprint + empty cache → wrongly fresh" $ do
      -- Simulate: test "foo" has no fingerprint, cache is empty
      let fps   = Map.empty :: Map.Map String Int
          cache = Map.empty :: Map.Map String Int
          key   = "foo"
          -- Old logic: Nothing /= Nothing = False → NOT stale (bug!)
          oldIsStale = Map.lookup key fps /= Map.lookup key cache
      oldIsStale @?= False  -- demonstrates the bug

  , testCase "new logic: missing fingerprint + empty cache → correctly stale" $ do
      let fps   = Map.empty :: Map.Map String Int
          cache = Map.empty :: Map.Map String Int
          key   = "foo"
          -- New logic: no fingerprint → always stale
          newIsStale = case Map.lookup key fps of
                         Nothing -> True
                         Just fp -> Map.lookup key cache /= Just fp
      newIsStale @?= True

  , testCase "old logic: missing fingerprint + stale cache → correctly stale" $ do
      -- If a previous fingerprint exists in cache but current is missing,
      -- the old logic happens to get this right: Nothing /= Just x = True
      let fps   = Map.empty :: Map.Map String Int
          cache = Map.singleton "foo" (42 :: Int)
          key   = "foo"
          oldIsStale = Map.lookup key fps /= Map.lookup key cache
      oldIsStale @?= True

  , testCase "matching fingerprint → correctly fresh" $ do
      let fps   = Map.singleton "foo" (42 :: Int)
          cache = Map.singleton "foo" (42 :: Int)
          key   = "foo"
          newIsStale = case Map.lookup key fps of
                         Nothing -> True
                         Just fp -> Map.lookup key cache /= Just fp
      newIsStale @?= False

  , testCase "changed fingerprint → correctly stale" $ do
      let fps   = Map.singleton "foo" (99 :: Int)
          cache = Map.singleton "foo" (42 :: Int)
          key   = "foo"
          newIsStale = case Map.lookup key fps of
                         Nothing -> True
                         Just fp -> Map.lookup key cache /= Just fp
      newIsStale @?= True
  ]

-- ---------------------------------------------------------------------------
-- 2. leafMap collision: duplicate leaf names
--
-- leafMap = Map.fromListWith const [ (last p, p) | ... ]
--
-- Two tests with the same leaf name in different groups collide.  Only one
-- path is kept; the other gets no fingerprint → falls into bug #1.

leafMapTests :: TestTree
leafMapTests = testGroup "leafMap collision"
  [ testCase "duplicate leaf name drops one path" $ do
      let paths :: [[String]]
          paths = [ ["group A", "add"]   -- leaf name = "add"
                  , ["group B", "add"]   -- same leaf name!
                  ]
          leafMap = Map.fromListWith const
            [ (last p, p) | p <- paths, not (null p) ]
      -- Only one entry survives
      Map.size leafMap @?= 1

  , testCase "dropped test gets no fingerprint key" $ do
      let paths :: [[String]]
          paths = [ ["group A", "add"]
                  , ["group B", "add"]
                  ]
          leafMap = Map.fromListWith const
            [ (last p, p) | p <- paths, not (null p) ]
          -- Simulate fingerprinting: only the surviving path gets a key
          fps = Map.fromList
            [ (pathKey path, 42 :: Int)
            | (_leafName, path) <- Map.toList leafMap
            ]
      -- The dropped path has no fingerprint
      let droppedKey = case Map.lookup "add" leafMap of
            Just kept -> case [ p | p <- paths, p /= kept ] of
                           (other:_) -> pathKey other
                           []        -> error "impossible: no other path"
            Nothing   -> error "impossible: no entry"
      Map.member droppedKey fps @?= False
  ]

-- ---------------------------------------------------------------------------
-- 3. findExprEnd stops at blank lines
--
-- countIndent returns 0 for a blank line.  findExprEnd stops when
-- countIndent pos <= baseIndent.  So a blank line inside a multi-line
-- expression truncates the body hash — edits after the blank line are
-- invisible to the cache.

findExprEndTests :: TestTree
findExprEndTests = testGroup "findExprEnd blank-line truncation"
  [ testCase "blank line inside do-block truncates body" $ do
      -- Simulates a test body like:
      --     testCase "foo" $ do
      --       let x = helper1
      --                          ← blank line
      --       x @?= expected
      let src = BSC.pack $ unlines
            [ "    testCase \"foo\" $ do"       -- line 0 (offset 0), indent 4
            , "      let x = helper1"           -- line 1, indent 6
            , ""                                -- line 2, blank
            , "      x @?= expected"            -- line 3, indent 6
            ]
          lineStart  = 0
          baseIndent = countIndent src 0  -- 4
          exprEnd    = findExprEnd src lineStart baseIndent
          bodyBytes  = BSC.take exprEnd src
      -- The blank line causes findExprEnd to stop before "x @?= expected"
      assertBool "body should NOT contain the assertion after the blank line"
        (not $ BSC.isInfixOf (BSC.pack "expected") bodyBytes)

  , testCase "without blank line, full body is captured" $ do
      let src = BSC.pack $ unlines
            [ "    testCase \"foo\" $ do"
            , "      let x = helper1"
            , "      x @?= expected"
            ]
          lineStart  = 0
          baseIndent = countIndent src 0  -- 4
          exprEnd    = findExprEnd src lineStart baseIndent
          bodyBytes  = BSC.take exprEnd src
      assertBool "body should contain the full expression"
        (BSC.isInfixOf (BSC.pack "expected") bodyBytes)

  , testCase "blank line in function definition truncates dependency tracking" $ do
      -- A library function with a blank line:
      --   myHelper x =
      --       let a = dep1 x
      --
      --           b = dep2 a
      --       in a + b
      --
      -- usedInBody uses findExprEnd from the start of the declaration,
      -- so it would stop at the blank line and miss dep2.
      let src = BSC.pack $ unlines
            [ "myHelper x ="                -- line 0, indent 0
            , "    let a = dep1 x"          -- line 1, indent 4
            , ""                            -- line 2, blank
            , "        b = dep2 a"          -- line 3, indent 8
            , "    in a + b"                -- line 4, indent 4
            ]
          lineStart  = 0
          baseIndent = countIndent src 0  -- 0
          exprEnd    = findExprEnd src lineStart baseIndent
          bodyBytes  = BSC.take exprEnd src
      -- dep2 is after the blank line — invisible to the cache
      assertBool "dep2 after blank line is NOT included in body"
        (not $ BSC.isInfixOf (BSC.pack "dep2") bodyBytes)
  ]

-- ---------------------------------------------------------------------------
-- 4. Instance resolution / overloaded identifiers
--
-- The dependency BFS matches identifiers by *occurrence name* (a bare
-- string).  This means an instance method @m@ is followed only if some
-- declaration on the BFS path textually mentions the name @m@.
--
-- For most class methods this works by accident: a polymorphic helper that
-- uses @m@ at all will textually contain the string @"m"@, and the BFS will
-- visit every binding named @m@ in every file — including the relevant
-- instance method.  (This is the source of the documented `occName`
-- collision false positive.)
--
-- The blind spot is methods invoked *implicitly* through type-class
-- resolution rather than by name in source.  The clearest example is
-- numeric literals: @poly x = x * x + 1@ uses @fromInteger@ at the call
-- site whenever @poly@ is specialised at a @Num@ instance, but the string
-- @"fromInteger"@ does not appear in @poly@'s body.  Editing @fromInteger@
-- in the relevant @Num@ instance can break the test without changing any
-- name on the BFS path, so the cache wrongly serves the stale result.
--
-- The test below models the BFS in pure code and asserts that, given a
-- test body that names @poly@ and a @poly@ whose used-name set is
-- @{*, +}@, the reachable name set excludes @fromInteger@ and @negate@ —
-- so changes to those instance methods are invisible to the cache.

instanceResolutionTests :: TestTree
instanceResolutionTests = testGroup "Instance resolution staleness"
  [ testCase "name-only BFS misses instance methods not textually reachable" $ do
      -- Names directly used in the test body `poly (Foo 3) @?= Foo 10`:
      let testBodyNames = Set.fromList ["poly", "Foo"]

      -- Decl map: name → set of names used in body (modelling ddUsedNames).
      -- `poly`'s body is `x * x + 1` — uses `*` and `+`; the `1` is a
      -- literal with no textual name.
      let declMap = Map.fromList
            [ ("poly",        Set.fromList ["*", "+"])
            -- The instance methods on `Num Foo` are each keyed by their
            -- occurrence name in `buildDeclMap`:
            , ("*",           Set.empty)
            , ("+",           Set.empty)
            , ("fromInteger", Set.empty)
            , ("negate",      Set.empty)
            ]

      let reachable = bfsReachable declMap testBodyNames

      -- `*` and `+` are reached (textually used by `poly`):
      assertBool "* is reachable"
        (Set.member "*" reachable)
      assertBool "+ is reachable"
        (Set.member "+" reachable)
      -- But `fromInteger` is NOT reached, even though it is invoked at
      -- every numeric-literal occurrence in `poly`'s body when specialised
      -- at `Foo`.  Changes to `instance Num Foo` { `fromInteger` } go
      -- undetected — false negative.
      assertBool "fromInteger is NOT reachable (false negative)"
        (not (Set.member "fromInteger" reachable))
      -- Same for any instance method whose name does not textually appear
      -- on a BFS path — `negate`, `abs`, `signum`, ...
      assertBool "negate is NOT reachable (false negative)"
        (not (Set.member "negate" reachable))

  , testCase "Polymorphic test body that names a method does follow it" $ do
      -- Sanity check: when the polymorphic helper *does* mention a method
      -- name, the BFS does follow it (this is why most cases happen to
      -- work by accident).  A helper `f x = negate x` keyed at "f", with
      -- an instance method "negate", is reachable from a test body that
      -- names "f".
      let testBodyNames = Set.fromList ["f"]
          declMap = Map.fromList
            [ ("f",      Set.fromList ["negate"])
            , ("negate", Set.empty)
            , ("abs",    Set.empty)
            ]
          reachable = bfsReachable declMap testBodyNames
      assertBool "negate (textually used by f) is reachable"
        (Set.member "negate" reachable)
      -- abs is still not reached: not textually mentioned anywhere.
      assertBool "abs (not textually used) is still unreachable"
        (not (Set.member "abs" reachable))
  ]

-- A pure model of the @transitiveDeps@ BFS: starting from @start@, visit
-- every name reachable through @declMap@ (name → names used in its body).
bfsReachable :: Map.Map String (Set String) -> Set String -> Set String
bfsReachable declMap = go Set.empty
  where
    go visited frontier
      | Set.null frontier = visited
      | otherwise =
          let visited'   = Set.union visited frontier
              nextNames  = Set.unions
                [ Map.findWithDefault Set.empty n declMap
                | n <- Set.toList frontier
                ]
              frontier'  = Set.difference nextNames visited'
          in go visited' frontier'
