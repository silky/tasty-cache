-- | Tests demonstrating false-negative scenarios in the caching logic.
--
-- A false negative means a test is served from cache (instant pass) when it
-- should have been re-run.  Each test group below isolates one mechanism by
-- which this can happen.
module FalseNegatives (falseNegativeTests) where

import qualified Data.ByteString.Char8        as BSC
import qualified Data.Map.Strict              as Map
import           Test.Tasty
import           Test.Tasty.HUnit

import           Test.Tasty.HieCache.Internal (countIndent, findExprEnd,
                                               pathKey)

falseNegativeTests :: TestTree
falseNegativeTests = testGroup "False-negative scenarios"
  [ stalenessTests
  , leafMapTests
  , findExprEndTests
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
